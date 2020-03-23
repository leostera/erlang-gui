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
  gen_server:start_link({local, ?MODULE}, ?MODULE, _Args, []).

init(_Args) ->
  {ok, #{ timer => set_timer(), acc => {0, right}, ref => none }}.

terminate(_, _) -> ok.

handle_cast(pause, #{ timer := T }=State) ->
  timer:cancel(T),
  {noreply, State#{ timer => none }};
handle_cast(start, #{ timer := none }=State) ->
  {noreply, State#{ timer => set_timer() }};
handle_cast(start, #{ timer := TRef }=State) ->
  timer:cancel(TRef),
  {noreply, State#{ timer => set_timer() }};
handle_cast(tick, #{ acc := {X, Dir}, ref := Ref }=State) ->
  {Pos, Picture} = draw(X),
  Ref2 = render(Ref, Pos, Picture),
  Acc = animate(X, Dir),
  {noreply, State#{ acc => Acc, ref => Ref2 }}.


set_timer() -> timer:apply_interval(30, ?MODULE, tick, []).

tick() -> gen_server:cast(?MODULE, tick).
pause() -> gen_server:cast(?MODULE, pause).
start() -> gen_server:cast(?MODULE, start).

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

render(none, Pos, Pic) ->
  {ok, Ref} = chalk:render(Pos, Pic),
  Ref;
render(Ref, Pos, Pic) ->
  ok = chalk:render(Ref, Pos, Pic),
  Ref.
