-module(cb_animated).

-behaviour(gen_server).

-export([ start_link/1
        , init/1
        , terminate/2
        , handle_call/3
        ]).

start_link(_Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, _Args, []).

init(_Args) ->
  {ok, Ref} = chalk_pipeline:register(fun () -> gen_server:call(?MODULE, draw) end),
  {ok, #{ acc => {0, right}, ref => Ref }}.

terminate(_, _) -> ok.

handle_call(draw, _From, #{ acc := {X, Dir} }=State) ->
  {Pos, Picture} = draw(X),
  Acc = animate(X, Dir),
  {reply, {ok, Picture}, State#{ acc => Acc }}.

animate(X, right) when X > 2000 -> {X, left};
animate(X, left ) when X < 0    -> {X, right};
animate(X, right) -> {X+10, right};
animate(X, left ) -> {X-10, left}.

draw(X) ->
  Canvas = sk_canvas:new(100, 100),

  Paint = sk_paint:new(),
  sk_paint:set_stroke_width(Paint, 40.0),
  sk_paint:set_color(Paint, sk_color:red()),
  Rect = sk_rect:make_xywh(0, 0, 100, 100),
  sk_canvas:draw_rect(Canvas, Rect, Paint),

  Picture = sk_picture:from_canvas(Canvas),
  {{X*1.0, 300.0, 4.0}, Picture}.
