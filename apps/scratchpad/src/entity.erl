-module(entity).

-behavior(gen_server).

-export([ start_link/2
        , init/1
        , terminate/2
        , handle_cast/2
        , handle_call/3
        ]).

%%==============================================================================
%% Behavior callbacks
%%==============================================================================

start_link(Args, Opts) -> gen_server:start_link(?MODULE, Args, Opts).

init(Args) -> {ok, initial_state(Args)}.

terminate(_, _) -> ok.

handle_call(_Msg, _From, State) -> {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

%%==============================================================================
%% Api
%%==============================================================================

draw() -> gen_server:call(?MODULE, draw).

%%==============================================================================
%% Internal
%%==============================================================================

initial_state(_Args) -> #{}.
