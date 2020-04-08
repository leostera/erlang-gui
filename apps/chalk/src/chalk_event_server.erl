-module(chalk_event_server).

-compile({no_auto_import,[register/1, register/2]}).

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
        , register/2
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

handle_cast({send, Event}, State) ->
  NewState = do_handle_event(Event, State),
  {noreply, NewState};
handle_cast(_Msg, State) -> {noreply, State}.

handle_call({register_matcher, M}, _From, State) -> do_register_matcher(M, State);
handle_call({register_node, M}, _From, State) -> do_register_node(M, State);
handle_call(dump, _From, State) -> {reply, State, State};
handle_call(_Msg, _From, State) -> {noreply, State}.

handle_info(_, State) -> {noreply, State}.

%%==============================================================================
%% Api
%%==============================================================================

dump() -> gen_server:call(?MODULE, dump).

send(Event) -> gen_server:cast(?MODULE, {send, Event}).

register(F) when is_function(F) -> register(all, F).

register(EventType, F) when is_function(F) and is_atom(EventType) ->
  true = chalk_event:is_valid_type(EventType),
  gen_server:call(?MODULE, {register_matcher, {self(), EventType, F}}, infinity);

register(NodeRef, F) when is_function(F) and is_reference(NodeRef) ->
  gen_server:call(?MODULE, {register_node, {self(), NodeRef, F}}, infinity).

%%==============================================================================
%% Internal
%%==============================================================================

initial_state() ->
  #{ handlers_by_event => cets:new(2, <<"chalk_event_server_handlers_by_event_table">>)
   , handlers_by_node => cets:new(2, <<"chalk_event_server_handlers_by_node_table">>)
   , node_tree => chalk_node_tree:dump()
   }.

do_register_matcher({Pid, EventName, Fn}, #{ handlers_by_event := Table }=State) ->
  cets:insert(Table, {{EventName, Pid}, Fn}),
  {reply, ok, State}.

do_register_node({Pid, NodeRef, Fn}, #{ handlers_by_node := Table }=State) ->
  cets:insert(Table, {{NodeRef, Pid}, Fn}),
  {reply, ok, State}.

do_handle_event(Event={_Ts, {{type, resized}, [{w,W},{h,H}]}}, State) ->
  NewState = State#{ size => {W, H} },
  fanout(Event, NewState),
  NewState;

do_handle_event({Ts, {{type, cursor_moved}, [{x,X},{y,Y}]}}
                 , State=#{ size := {W, H} }) ->
  {X2, Y2} = scale_coords({W,H}, {X,Y}),
  Event = {Ts, {{type, cursor_moved}, [{x, X2}, {y, Y2}]}},
  fanout(Event, State),
  State;

do_handle_event(Event, State) ->
  fanout(Event, State),
  State.

%%==============================================================================
%% Utilities
%%==============================================================================

fanout(Event, State) ->
  case chalk_event:has_position(Event) of
    true -> fanout_by_node(Event, State);
    _ -> ok
  end,
  fanout_by_event(Event, State).

fanout_by_event(Event, #{ handlers_by_event := Table }) ->
  EvType = chalk_event:type(Event),
  cets:foldl(
    fun ({Key={RegEvType, Pid}, Handler}, _)  when (RegEvType =:= EvType )
                                                or (RegEvType =:= all)   ->
        Alive = is_process_alive(Pid),
        Result = case Alive of
                   true -> call_handler(Event, Handler);
                   false -> remove_handler(Key, Table)
                 end,
        case Result of
          {error, _Reason} -> remove_handler(Key, Table);
          _ -> ok
        end;
        (_, _) -> ok
    end, ok, Table).

fanout_by_node(Event, #{ handlers_by_node := NodeTable, node_tree := Nodes }) ->
  {ok, Position} = chalk_event:position(Event),
  cets:foldl(
    fun ({Key={Node, Pid}, Handler}, _) ->
        case chalk_node_tree:rect(Node, Nodes) of
          {ok, Rect} ->
            Hit = eu_rect:is_point_within(Rect, Position),
            Alive = is_process_alive(Pid),
            Result = case {Alive, Hit} of
                       {true, true} -> call_handler(Event, Handler);
                       {false, _} -> remove_handler(Key, NodeTable);
                       _  -> handler_ignored
                     end,
            case Result of
              {error, _Reason} -> remove_handler(Key, NodeTable);
              _ -> ok
            end;
          _ ->
            node_not_yet_rendered
        end
    end, ok, NodeTable).

call_handler(Event, Handler) ->
  case (catch Handler(Event)) of
    {'EXIT', Reason} -> {error, Reason};
    _ -> ok
  end.

remove_handler(Key, Table) ->
  ok = cets:delete(Table, Key),
  ok.


scale_coords({W, H}, {X, Y}) ->
  MaxW = 1920,
  MaxH = 1080,
  { round(MaxW/W * X), round(MaxH / H * Y) }.

