-module(hw_animated).

-behaviour(gen_server).

-export([ start_link/1
        , init/1
        , terminate/2
        , handle_cast/2
        , pause/0
        , start/0
        , tick/0
        ]).

start_link(_Args) ->
  gen_server:start_link(?MODULE, _Args, []).

init(_Args) ->
  {ok, #{ timer => set_timer(), acc => {0, right} }}.

terminate(_, _) -> ok.

handle_cast(pause, #{ timer := T }=State) ->
  timer:cancel(T),
  {noreply, State#{ timer => none }};
handle_cast(start, #{ timer := none }=State) ->
  {noreply, State#{ timer => set_timer() }};
handle_cast(tick, #{ acc := {X, Dir} }=State) ->
  draw(X),
  Acc = animate(X, Dir),
  {noreply, State#{ acc => Acc }}.


set_timer() -> timer:apply_interval(16, ?MODULE, tick, []).

tick() -> gen_server:cast(?MODULE, tick).
pause() -> gen_server:cast(?MODULE, pause).
start() -> gen_server:cast(?MODULE, start).

animate(X, right) when X > 2000 -> {X, left};
animate(X, left ) when X < 0    -> {X, right};
animate(X, right) -> {X+10, right};
animate(X, left ) -> {X-10, left}.

draw(X) ->
  io:format("hw_animated:draw/1"),
  Canvas = sk_canvas:new(100, 100),
  sk_canvas:draw_color(Canvas, sk_color:cyan()),

  Paint = sk_paint:new(),
  sk_paint:set_stroke_width(Paint, 40.0),
  sk_paint:set_color(Paint, sk_color:red()),
  Rect = sk_rect:make_xywh(0, 0, 100, 100),
  sk_canvas:draw_rect(Canvas, Rect, Paint),

  Paint2 = sk_paint:new(),
  sk_paint:set_color(Paint2, sk_color:blue()),
  Font = sk_font:new([{size, 300.0}]),
  Text = sk_text_blob:from_binary("Hello, Joe!", Font),
  sk_canvas:draw_text_blob(Canvas, Text, 500, 350, Paint2),

  Picture = sk_picture:from_canvas(Canvas),
  canvas_server:draw({X, 300}, Picture).
