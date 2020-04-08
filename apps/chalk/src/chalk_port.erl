-module(chalk_port).

-behavior(gen_server).

-define(CHALK_PORT, code:priv_dir(chalk) ++ "/native/chalk_port").

-define(MAX_FRAME_REQUEST_AGE, 16 * 1000000).
-define(MAX_EVENT_AGE, 16 * 1000000).

-export([ start_link/2
        , stop/0
        , init/1
        , terminate/2
        , handle_info/2
        , handle_cast/2
        , handle_call/3
        ]).

-export([ dump/0 ]).

%%==============================================================================
%% Behavior callbacks
%%==============================================================================

start_link(Args, Opts) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

stop() ->
  gen_server:stop(?MODULE).

init(_Args) ->
  erlang:system_flag(scheduler_bind_type, processor_spread),
  process_flag(trap_exit, true),
  process_flag(priority, max),
  {Writer, Reader} = prepare_files(),
  Port = open_port({spawn, ?CHALK_PORT}, [binary, {parallelism, false}]),
  {ok, #{ port => Port
        , port_info => maps:from_list(erlang:port_info(Port))
        , writer => Writer
        , write_offset => 0
        , reader => Reader
        }
  }.

terminate(_, State) -> do_graceful_termination(State), ok.

handle_info({_Port, {data, Ev}}, State) -> do_handle_data(Ev, State);
handle_info({'EXIT', _Port, _Reason}, State) -> do_graceful_termination(State);
handle_info(_, State) -> {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_call(dump, _, State) -> {reply, State, State};
handle_call(_, _, State) -> {noreply, State}.

%%==============================================================================
%% Api
%%==============================================================================

dump() -> gen_server:call(?MODULE, dump).

%%==============================================================================
%% Internal
%%==============================================================================

do_graceful_termination(#{ reader := Reader
                         , port := Port
                         , port_info := #{ os_pid := OSPid } }=State) ->
  memmap_port:port_close(Reader),
  port_close(Port),
  os:cmd("kill -9 " ++ integer_to_list(OSPid)),
  clean_files(),
  {noreply, State}.


do_handle_data(Ev, State) ->
  RecvAt = erlang:system_time(),
  NewState = case (catch handle_event(RecvAt, Ev, State)) of
               {'EXIT', Error} ->
                 io:format("ERROR: ~p\n", [Error]),
                 State;
               S -> S
             end,
  {noreply, NewState}.

handle_event(RecvAt, RawEv, State) when is_binary(RawEv) ->
  {T0, Event} = binary_to_term(RawEv),
  Age = RecvAt - T0,
  dispatch_event(Age, Event, State).

%% Render events
dispatch_event( Age
              , { {ts, _} , {kind, command} , {command, request_frame}}
              , State=#{ writer := Writer , write_offset := Offset }
              ) when Age < ?MAX_FRAME_REQUEST_AGE ->
  {ok, Frame} = chalk_render_pipeline:flush(),
  NewOffset = do_command(Writer, Offset, render, Frame),
  State#{ write_offset => NewOffset };

%% Relay Events
dispatch_event( Age
              , {{ts, HappenedAt}, {kind, relay}, {data, Event}}
              , State
              ) when Age < ?MAX_EVENT_AGE ->
  ok = chalk_event_server:send({HappenedAt, Event}),
  State;

%% Ignore the rest
dispatch_event(_, _, State) -> State.

do_command(Writer, Offset, Kind, Data) ->
  Cmd = {{ref,0}, {kind,Kind}, {data,Data}},
  {ok, NewOffset} = memmap:write_term(Writer, Cmd, Offset),
  NewOffset.


%%==============================================================================
%% Utilities
%%==============================================================================

write_path() -> <<"/home/ostera/repos/github.com/ostera/erlang-gui/chalk.port_in.mmap">>.
read_path() -> <<"/home/ostera/repos/github.com/ostera/erlang-gui/chalk.port_out.mmap">>.

clean_files() ->
  ok = case file:delete(write_path()) of
         {error, enoent} -> ok;
         ok -> ok
       end,
  ok = case file:delete(read_path()) of
         {error, enoent} -> ok;
         ok -> ok
       end,
  ok.

prepare_files() ->
  clean_files(),
  Size = 1024*1024*1024,
  Writer = memmap:open_write(write_path(), Size),
  {ok, Reader} = memmap_port:open_port(self(), read_path(), Size),
  {Writer, Reader}.
