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
  (catch handle_event(T0, Ev, State)),
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
  %io:format("rust:event_dispatching:\t~pms\n", [(Ts - HappenedAt)/1000000]),
  %io:format("erlang:port_receive:\t\t\t~pms\n", [(T0-Ts)/1000000]),
  %io:format("erlang:binary_to_term:\t\t\t~pms\n", [(T1-T0)/1000000]),
  dispatch_event({T0, T1}, {Ts, Event}, State),
  ok.

%% Render events
dispatch_event(_, {_, {{ts, _}, {kind, notice}, {data, render_queued}}}, _) ->
  ok;
dispatch_event(_, {_, {{ts, _}, {kind, command}, {command, request_frame}}}, State) ->
  do_render(State);

%% Droppable events
dispatch_event(_, {_,{{ts, _}, {kind, relay}, {data, device_event}}}, _) ->
  ok;

%% Relay Events
dispatch_event({T0,T1}, {Ts, {{ts, HappenedAt}, {kind, relay}, {data, E}}}, State) ->
  do_forward_event({HappenedAt, E}, State),
  T2 = erlang:system_time(),
  %io:format("chalk_port:dispatch_event/2:\t\t~pms\n", [(T2 - T1)/1000000]),
  ok;

%% Ignore the rest
dispatch_event(_, _, _) -> ok.


do_forward_event(Event, _State) ->
  chalk_event_server:send(Event).

do_render(#{ port := Port }) ->
  Frame = chalk_pipeline:flush(),
  ok = do_command(Port, render, Frame).

do_command(Port, Kind, Data) ->
  Ref = make_ref(),
  Cmd = {{ref,Ref}, {kind,Kind}, {data,Data}},
  Msg = term_to_binary(Cmd),
  port_command(Port, Msg),
  ok.
