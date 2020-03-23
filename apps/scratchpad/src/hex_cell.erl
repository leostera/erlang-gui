-module(hex_cell).

-behavior(gen_server).

-export([ start_link/2
        , init/1
        , terminate/2
        , handle_cast/2
        , handle_call/3
        ]).

-export([ draw/1
        ]).

%%==============================================================================
%% Behavior callbacks
%%==============================================================================

start_link(Args, Opts) -> gen_server:start_link(?MODULE, Args, Opts).

init(Args) ->
  State = initial_state(Args),
  Self = self(),
  %chalk_event_server:register(Self),
  chalk_pipeline:register(fun () -> hex_cell:draw(Self) end),
  {ok, State}.

terminate(_, _) -> ok.

handle_call(draw, _From, State) -> do_draw(State);
handle_call(_Msg, _From, State) -> {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

%%==============================================================================
%% Api
%%==============================================================================

draw(Pid) -> gen_server:call(Pid, draw).

%%==============================================================================
%% Internal
%%==============================================================================

initial_state(S) -> S.

do_draw(#{ pos := {X, Y, Z}, tile := T }=State) ->
  {reply, {ok, {X, Y, Z}, T}, State}.
