-module(animated_cubic).

-behavior(gen_server).

-export([ start_link/2
        , init/1
        , terminate/2
        , handle_cast/2
        , handle_call/3
        ]).

-export([ draw/0, dump/0, restart/0, start/0 ]).

start() ->
  animated_cubic:start_link([],[]),
  chalk_pipeline:flush().

restart() ->
  gen_server:stop(animated_cubic),
  chalk_pipeline:clear(),
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
   , last_steps => 0
   }.

do_init(State) ->
  chalk:add_node(fun () -> {ok, {0.0,0.0,0.0}, hex_lib:bg()} end),
  chalk:add_node(fun () -> animated_cubic:draw() end),
  State.

do_draw(#{ pos := Pos
         , last_time := LastT
         , last_steps := LastStep
         }=State) ->
  Now = erlang:system_time(),
  Steps = step_count_since(LastT, Now),
  NextSteps = LastStep + Steps,

  Canvas = sk_canvas:new(1000, 1000),

  Path = sk_path:new(),
  sk_path:move_to(Path, 0.0, 0.0),
  { P2, P3 } = animate(NextSteps),
  sk_path:cubic_to(Path, {0, 0}, P2, P3),

  sk_canvas:draw_path(Canvas, Path, line_paint(NextSteps)),
  Picture = sk_picture:from_canvas(Canvas),

  NewState = State#{ last_time => case Steps of
                                    0 -> LastT;
                                    _  -> Now
                                  end
                   , last_steps => NextSteps
                   },
  {reply, {ok, Pos, Picture}, NewState}.


animate(T) -> { {10*T,22*T}
              , {round(math:sqrt(T)),200}
              }.


step_count_since(T, Now) ->
  Delta = (Now - T) / (10*1000*1000),
  round(Delta).

line_paint(T) ->
  Paint = sk_paint:new(),
  sk_paint:set_style(Paint, sk_paint:style_stroke_and_fill()),
  sk_paint:set_stroke_width(Paint, 10.0),
  sk_paint:set_color(Paint, color(T)),
  Paint.

color(T) -> sk_color:rgba(loop(T),loop(T*T),loop(T/2.0),255).

loop(T) when T > 255 -> loop(T / 255.0);
loop(T) -> round(T).
