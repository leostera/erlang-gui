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
        , dump/0
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
  process_flag(priority, high),
  {ok, initial_state()}.

terminate(_, _) -> ok.

handle_cast(_Msg, State) -> {noreply, State}.

handle_call({send, Event}, _From, State) -> do_handle_event(Event, State);
handle_call({register, H}, _From, State) -> do_register(H, State);
handle_call(dump, _From, State) -> {reply, State, State};
handle_call(_Msg, _From, State) -> {noreply, State}.

handle_info(_, State) ->
  {ok, State}.

%%==============================================================================
%% Api
%%==============================================================================

dump() -> gen_server:call(?MODULE, dump).

send(Event) -> gen_server:call(?MODULE, {send, Event}).

register(F) when is_function(F) -> gen_server:call(?MODULE, {register, {self(), F}}).

%%==============================================================================
%% Internal
%%==============================================================================

initial_state() ->
  #{ handlers => cets:new( 2
                         , <<"chalk_event_server">>
                         , [{write_concurrency, true}, {read_concurrency, true}, ordered_set])
   }.

do_register(Handler, #{ handlers := Table }=State) ->
  cets:insert(Table, Handler),
  {reply, ok, State}.

do_handle_event(Event={_Ts, {{type, resized}, [{w,W},{h,H}]}}, State) ->
  (catch fanout(Event, State)),
  {reply, ok, State#{ size => {W, H} }};

do_handle_event({Ts, {{type, cursor_moved}, [{x,X},{y,Y}]}}
                , State=#{ size := {W, H} }) ->
  {X2, Y2} = scale_coords({W,H}, {X,Y}),
  Event = {Ts, {{type, cursor_moved}, [{x, X2}, {y, Y2}]}},
  %io:format("chalk_event_server:do_handle_event/2:\t~pms\n", [(erlang:system_time() - Ts)/1000000]),
  (catch fanout(Event, State)),
  {reply, ok, State};

do_handle_event(Event, State) ->
  (catch fanout(Event, State)),
  {reply, ok, State}.

%%==============================================================================
%% Utilities
%%==============================================================================

fanout(Event, #{ handlers := Table }) ->
  Cleanup = cets:foldl(fun ({Pid, Handler}, DeadPids) ->
                           case is_process_alive(Pid) of
                             true -> (catch Handler(Event)), DeadPids;
                             false -> [Pid | DeadPids]
                           end
                       end, [], Table),
  [ cets:delete(Table, Pid) || Pid <- Cleanup ],
  ok.


scale_coords({W, H}, {X, Y}) ->
  MaxW = 3840,
  MaxH = 2160,
  { round(MaxW/W * X), round(MaxH / H * Y) }.

