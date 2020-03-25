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

handle_cast({send, Event}, State) -> {noreply, do_handle_event(Event, State)}.

handle_call({register, H}, _From, #{ handlers := Hs }=State) ->
  {reply, ok, State#{ handlers => [H|Hs] }}.

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

do_handle_event({{type, resized}, [{w,W},{h,H}]}, State) ->
  State#{ size => {W, H} };
do_handle_event({{type, cursor_moved}, [{x,X},{y,Y}]}, State=#{ size := {W, H} }) ->
  {X2, Y2} = scale_coords({W,H}, {X,Y}),
  Event = {{type, cursor_moved}, [{x, X2}, {y, Y2}]},
  fanout(Event, State),
  State;
do_handle_event(Event, State) ->
  fanout(Event, State),
  State.

fanout(Event, #{ handlers := Hs }) ->
  [ (catch gen_server:cast(H, Event)) || H <- Hs ].

scale_coords({W, H}, {X, Y}) ->
  MaxW = 3840,
  MaxH = 2160,
  { MaxW/W * X, MaxH / H * Y }.

