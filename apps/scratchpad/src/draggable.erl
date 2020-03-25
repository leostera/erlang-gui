-module(draggable).

-behavior(gen_server).

-export([ start_link/2
        , init/1
        , terminate/2
        , handle_cast/2
        , handle_call/3
        ]).

-export([ draw/0
        , start/0
        , restart/0
        ]).

%%==============================================================================
%% Behavior callbacks
%%==============================================================================

start_link(Args, Opts) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

init(Args) ->
  State = initial_state(Args),
  Self = self(),
  chalk_event_server:register(Self),
  chalk_pipeline:register(fun () -> {ok, {0.0,0.0,0.0}, hex_lib:bg()} end),
  chalk_pipeline:register(fun () -> draggable:draw() end),
  {ok, State}.

terminate(_, _) -> ok.

handle_call(draw, _From, State) -> do_draw(State);
handle_call(_Msg, _From, State) -> {noreply, State}.

handle_cast({{type, mouse_input}, [{state,pressed}|_]}, State) ->
  chalk_pipeline:set_frame_rate(30.0),
  {noreply, State};
handle_cast({{type, cursor_moved}, [{x,X}, {y,Y}]}, State) ->
  {noreply, do_cursor_move(State, {X,Y})};
handle_cast(Msg={{type, _}, _}, State) ->
  io:format("~p\n",[Msg]),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

%%==============================================================================
%% Api
%%==============================================================================

start() ->
  draggable:start_link([],[]),
  chalk_pipeline:flush().

restart() ->
  gen_server:stop(draggable),
  chalk_pipeline:clear(),
  start().

draw() -> gen_server:call(?MODULE, draw).

%%==============================================================================
%% Internal
%%==============================================================================

initial_state(_) ->
  #{ hex => hex_lib:flat_hex_tile(100)
   , pos => {1000.0, 1000.0, 1.0}
   , time => erlang:system_time()
   }.

do_cursor_move(State, Pos={X, Y}) ->
  io:format("Updating position: ~p\n", [Pos]),
  State#{ pos => {X * 1.0, Y * 1.0, 1.0} }.

do_draw(#{ pos := Pos
         , hex := Hex
         , time := _
         }=State) ->
  Now = erlang:system_time(),
  {reply, {ok, Pos, Hex}, State#{ time => Now }}.
