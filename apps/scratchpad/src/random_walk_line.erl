-module(random_walk_line).

-behavior(gen_server).

-export([ start_link/2
        , init/1
        , terminate/2
        , handle_cast/2
        , handle_call/3
        ]).

-export([ draw/0, dump/0, restart/0, start/0 ]).

start() ->
  random_walk_line:start_link([],[]).

restart() ->
  gen_server:stop(random_walk_line),
  start().

%%==============================================================================
%% Behavior callbacks
%%==============================================================================

start_link(Args, Opts) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

init(Args) ->
  State = initial_state(Args),
  {ok, do_init(State)}.

terminate(_, _) -> ok.

handle_call(draw, _From, State) -> do_draw(State);
handle_call(dump, _From, State) -> {reply, State, State};
handle_call(_Msg, _From, State) -> {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

%%==============================================================================
%% Api
%%==============================================================================

dump() -> gen_server:call(?MODULE, dump).

draw() -> gen_server:call(?MODULE, draw).

%%==============================================================================
%% Internal
%%==============================================================================

initial_state(_) ->
  #{ pos => { 0.0, 0.0, 1.0 }
   , last_time => erlang:system_time()
   , last_pos => {1000.0,1000.0}
   , initial_pos => {1000.0,1000.0}
   , last_steps => [{1000.0,1000.0}]
   }.

do_init(State) ->
  chalk:add_node(fun random_walk_line:draw/0),
  State.

do_draw(#{ pos := Pos
         , last_time := LastT
         , last_steps := LastSteps
         , initial_pos := {X0, Y0}
         , last_pos := LastPos
         }=State) ->
  Now = erlang:system_time(),
  Steps = step_count_since(LastT, Now),

  NextSteps = [NextLastPos|_] = lists:foldl(
                fun (_, Acc) ->
                    [ animate(LastPos, random_dir()) | Acc]
                end
                , LastSteps
                , Steps),

  Canvas = sk_canvas:new(2000, 2000),

  Path = sk_path:new(),
  sk_path:move_to(Path, X0, Y0),
  lists:foreach(fun ({X, Y}) ->
                    sk_path:move_to(Path, X, Y),
                    sk_path:line_to(Path, X, Y)
                end, NextSteps),

  sk_canvas:draw_path(Canvas, Path, line_paint()),
  Picture = sk_picture:from_canvas(Canvas),

  NewState = State#{ last_time => case Steps of
                                    [] -> LastT;
                                    _  -> Now
                                  end
                   , last_steps => NextSteps
                   , last_pos => NextLastPos
                   },
  {reply, {ok, Pos, Picture}, NewState}.

move_speed() -> 100.0.

animate({X, Y}, up)    -> {X - move_speed(), Y};
animate({X, Y}, left)  -> {X, Y - move_speed()};
animate({X, Y}, down)  -> {X + move_speed(), Y};
animate({X, Y}, right) -> {X, Y + move_speed()}.

random_dir() -> random_dir(rand:uniform(4)).
random_dir(1) -> up;
random_dir(2) -> left;
random_dir(3) -> down;
random_dir(4) -> right.


step_count_since(T, Now) ->
  Delta = (Now - T) / (10*1000*1000),
  case round(Delta) div 10 of
    0 -> [];
    N -> lists:seq(0, N)
  end.

line_paint() ->
  Paint = sk_paint:new(),
  sk_paint:set_style(Paint, sk_paint:style_stroke()),
  sk_paint:set_stroke_width(Paint, 3.0),
  sk_paint:set_color(Paint, sk_color:rgba(0,0,0,255)),
  Paint.
