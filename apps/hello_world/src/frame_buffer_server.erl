-module(frame_buffer_server).

-behavior(gen_server).

-export([ start_link/1
        , stop/0
        , init/1
        , terminate/2
        , handle_call/3
        ]).

-export([ register/0
        , submit_frame/2
        , statistics/0
        , foreach/1
        ]).

%%===============================================================================
%% Behavior Callbacks
%%===============================================================================

initial_state() ->
  #{ frame_buffer => ets:new(frame_buffer, [named_table, ordered_set]) }.

init(_Args) -> {ok, initial_state()}.

terminate(_, _State) ->
  ok.

handle_call(statistics, _From, S0) ->
  {reply, do_statistics(S0), S0};

handle_call({submit_frame, Req}, _From, S0) ->
  ok = do_submit_frame(Req, S0),
  {reply, ok, S0};

handle_call(register, _From, S0) ->
  {ok, Ref} = do_register(S0),
  {reply, {ok, Ref}, S0}.


%%===============================================================================
%% Api
%%===============================================================================

start_link(_Args) -> gen_server:start_link({local, ?MODULE}, ?MODULE, _Args, []).

stop() -> gen_server:stop(?MODULE).

statistics() -> gen_server:call(?MODULE, statistics).

register() -> gen_server:call(?MODULE, register).

submit_frame(Ref, {{X,Y}, _Pic}=Frame) when is_float(X) and is_float(Y) -> gen_server:call(?MODULE, {submit_frame, {Ref, Frame}}).

foreach(F) -> ets:foldl(fun ({_Ref, Frame}, _) -> F(Frame) end, none, frame_buffer).

%%===============================================================================
%% Internal
%%===============================================================================

do_submit_frame({Ref, Frame}, #{ frame_buffer := FBuf }=_State) ->
  true = ets:insert(FBuf, {Ref, Frame}),
  ok.

do_register(#{ frame_buffer := FBuf }) ->
  Ref = make_ref(),
  true = ets:insert(FBuf, {Ref, none}),
  {ok, Ref}.

do_statistics(#{ frame_buffer := FBuf }) ->
  #{ frame_count => ets:info(FBuf, size) }.
