-module(chalk_event_server).

-behavior(gen_server).

-export([ start_link/2
        , stop/0
        , init/1
        , terminate/2
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        ]).

-export([ send/1
        , register/1
        ]).

%%==============================================================================
%% Behavior callbacks
%%==============================================================================

start_link(Args, Opts) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

stop() ->
  gen_server:stop(?MODULE).

init(_) ->
  {ok, #{ handlers => [] }}.

terminate(_, _) -> ok.

handle_cast({send, Event}, #{ handlers := Hs }=State) ->
  [ gen_server:cast(H, Event) || H <- Hs ],
  {noreply, State}.

handle_call({register, H}, _From, #{ handlers := Hs }) ->
  {reply, ok, #{ handlers => [H|Hs] }}.

handle_info(_, State) ->
  {ok, State}.

%%==============================================================================
%% Api
%%==============================================================================

send(Event) -> gen_server:abcast(?MODULE, {send, Event}).

register(H) -> gen_server:call(?MODULE, {register, H}).

%%==============================================================================
%% Internal
%%==============================================================================


