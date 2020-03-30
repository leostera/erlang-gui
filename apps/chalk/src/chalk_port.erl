-module(chalk_port).

-behavior(gen_server).

-define(CHALK_PORT, code:priv_dir(chalk) ++ "/native/chalk_port").

-export([ start_link/2
        , stop/0
        , init/1
        , terminate/2
        , handle_info/2
        , handle_cast/2
        , handle_call/3
        ]).

-export([ request_flush/0 ]).

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
  Port = open_port({spawn, ?CHALK_PORT}, [binary, {parallelism, false}]),
  {ok, #{ port => Port }}.

terminate(_, #{ port := Port }) ->
  port_close(Port),
  ok.

handle_info({_Port, {data, Ev}}, State) ->
  T0 = erlang:system_time(),
  case (catch handle_event(T0, Ev, State)) of
    {'EXIT', Err} -> io:format("~p\n", [Err]);
    _ -> ok
  end,
  {noreply, State};
handle_info({'EXIT', _Port, _Reason}, _State) ->
  {noreply, #{ port => none }}.

handle_cast(quit, #{ port := Port }=State) ->
  Ref = make_ref(),
  port_command(Port, term_to_binary({Ref, quit})),
  {noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.

handle_call(request_flush, _, State) ->
  do_render(State),
  {reply, ok, State};
handle_call(_, _, State) -> {noreply, State}.

%%==============================================================================
%% Api
%%==============================================================================

request_flush() ->
  spawn(fun () -> gen_server:call(?MODULE, request_flush) end).

%%==============================================================================
%% Internal
%%==============================================================================

handle_event(T0, RawEv, State) when is_binary(RawEv) ->
  {Ts, Event} = binary_to_term(RawEv),
  T1 = erlang:system_time(),
  %io:format("\n\n\nEVENT: \n\n~p\n\n\n", [{Ts, Event}]),
  case ((T1 - Ts)/1000000) > 2 of
    true -> ok;
    _ -> dispatch_event({T0, T1, byte_size(RawEv)}, {Ts, Event}, State)
  end.

%% Render events
dispatch_event(_, {_, {{ts, _}, {kind, notice}, {data, render_queued}}}, _) ->
  ok;
dispatch_event({T0, T1, Size}, {Ts, {{ts, HappenedAt}, {kind, command}, {command, request_frame}}}, State) ->
  Frame = chalk_pipeline:flush(),
  T2 = erlang:system_time(),
  {MsgSize, T3, T4} = do_render(Frame, State),
  %io:format("\n\nrust:event_dispatching:\t~pms\n", [(Ts - HappenedAt)/1000000]),
  %io:format("erlang:port_receive:\t~pms\n", [(T0-Ts)/1000000]),
  %{_, MsgCount} = process_info(whereis(chalk_port), message_queue_len),
  %io:format("port:message_queue_len:\t~p\n", [MsgCount]),

  %io:format("erlang:msg_byte_size:\t~p bytes\n", [Size]),
  %io:format("erlang:binary_to_term:\t~pms\n", [(T1-T0)/1000000]),
  %io:format("chalk_pipeline:flush/0:\t~pms\n", [(T2 - T1)/1000000]),
  %io:format("erlang:term_to_binary:\t~pms\n", [(T3-T2)/1000000]),
  %io:format("erlang:msg_byte_size:\t~p bytes\n", [MsgSize]),
  %io:format("erlang:port_command/1:\t~pms\n", [(T4 - T3)/1000000]),
  ok;

%% Droppable events
dispatch_event(_, {_,{{ts, _}, {kind, relay}, {data, device_event}}}, _) ->
  ok;

%% Relay Events
dispatch_event({T0,T1, Size}, {Ts, {{ts, HappenedAt}, {kind, relay}, {data, E}}}, State) ->
  ok = chalk_event_server:send({Ts, E}),
  T2 = erlang:system_time(),
  %io:format("\n\nrust:event_dispatching:\t~pms\n", [(Ts - HappenedAt)/1000000]),
  %io:format("erlang:msg_byte_size:\t~p bytes\n", [Size]),
  %io:format("erlang:port_receive:\t~pms\n", [(T0-Ts)/1000000]),
  %io:format("erlang:binary_to_term:\t~pms\n", [(T1-T0)/1000000]),
  %io:format("chalk_port:do_fw_evt/2:\t~pms\n", [(T2 - T1)/1000000]),
  ok;

%% Ignore the rest
dispatch_event(_, _, _) -> ok.


do_render(#{ port := Port }) ->
  Frame = chalk_pipeline:flush(),
  do_command(Port, render, Frame).
do_render(Frame, #{ port := Port }) ->
  do_command(Port, render, Frame).

do_command(Port, Kind, Data) ->
  Cmd = {{ref,0}, {kind,Kind}, {data,Data}},
  Msg = term_to_binary(Cmd),
  T0 = erlang:system_time(),
  port_command(Port, Msg),
  T1 = erlang:system_time(),
  {byte_size(Msg), T0, T1}.
