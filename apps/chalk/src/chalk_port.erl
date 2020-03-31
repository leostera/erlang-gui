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
  process_flag(priority, high),

  file:write_file(<<"./analysis/data/2020-03-30-memmap-port-recv-send-latency-realistic-load.csv">>,
  "rust:event_dispatching,erlang:port_receive,port:message_queue_len,erlang:msg_byte_size,erlang:binary_to_term,chalk_pipeline:flush/0,erlang:term_to_binary,erlang:msg_byte_size,erlang:port_command\n", [append]),

  Writer = memmap:open_write(
            <<"/home/ostera/repos/github.com/ostera/erlang-gui/chalk.port_in.mmap">>,
            1024*1024*1024
           ),

  {ok, Reader} = memmap_port:open_port(
                   self(),
                   <<"/home/ostera/repos/github.com/ostera/erlang-gui/chalk.port_out.mmap">>,
                   1024*1024*1024
                  ),


  Port = open_port({spawn, ?CHALK_PORT}, [binary, {parallelism, false}]),
  {ok, #{ port => Port
        , writer => Writer
        , write_offset => 0
        , reader => Reader
        , current_frame => none
        , last_frame_time => erlang:system_time()
        , framerate => 0
        , frame_count => 0
        }
  }.

terminate(_, #{ reader := Reader }) ->
  exit(Reader),
  ok.

handle_info({_Port, {data, Ev}}, State) ->
  T0 = erlang:system_time(),
  NewState = case (catch handle_event(T0, Ev, State)) of
               {'EXIT', Error} ->
                 io:format("ERROR: ~p\n", [Error]),
                 State;
               S -> S
             end,
  {noreply, NewState};
handle_info({'EXIT', _Port, _Reason}, _State) ->
  {noreply, #{ port => none }};
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

handle_event(T1, RawEv, State) when is_binary(RawEv) ->
  {T0, Event} = binary_to_term(RawEv),
  T2 = erlang:system_time(),
  %io:format("EVENT:\n~p\n\n",[Event]),
  dispatch_event({T0, T1, T2, byte_size(RawEv)}, Event, State).

dispatch_event( _
              , { _
                , {kind, relay}
                , {data, {{type, cursor_moved}, [{x,X}, {y,Y}]}}
                }
              , State=#{ frame_count := FPS }) ->
  C = sk_canvas:new(1920, 1080),
  sk_canvas:draw_color(C, sk_color:rgba(0,0,0,255)),
  Font = sk_font:default(),
  Text = lists:flatten(io_lib:format("(~p, ~p)", [round(X), round(Y)])),
  TextBlob = sk_text_blob:from_string(Text, Font),
  Paint = sk_paint:new(),
  Color = sk_color:rgba(40, 255, 40, 255),
  sk_paint:set_color(Paint, Color),
  sk_canvas:draw_text_blob(C, TextBlob, X, Y, Paint),

  Frame = sk_picture:from_canvas(C),
  T3 = erlang:system_time(),
  State#{ current_frame => {some, Frame, T3} };

dispatch_event( {T0, T1, T2, Size}
              , {{_, HappenedAt}, _, {_, _}}
              , State=#{ writer := Writer
                       , write_offset := Offset
                       , frame_count := Fps
                       , framerate := Framerate
                       , last_frame_time := LastFrameTime
                       , current_frame := {some, Frame, _}
                       }) when ((T1-T0)/100000) < 2 ->
  T3 = Now = erlang:system_time(),
  FpsCounter = fps:frame(Framerate),
  C = sk_canvas:new(1920, 1080),
  sk_canvas:draw_picture_at(C, {0.0, 0.0}, Frame),
  sk_canvas:draw_picture_at(C, {10.0, 200.0}, FpsCounter),
  Bytes = sk_picture:as_bytes(sk_picture:from_canvas(C)),

  {NewOffset, T4} = do_command(Writer, Offset, render, Bytes),
  {_, MsgCount} = process_info(whereis(chalk_port), message_queue_len),
  LogStr = lists:flatten(io_lib:format("~p,~p,~p,~p,~p,~f,~p\n", [
                                             (T0 - HappenedAt)/1000000,
                                             (T1-T0)/1000000,
                                             MsgCount,
                                             Size,
                                             (T2-T1)/1000000,
                                             (T3 - T2)/1000000,
                                             (T4-T3)/1000000
                                            ])),
  file:write_file(<<"./analysis/data/2020-03-30-memmap-port-recv-send-latency-realistic-load.csv">>,
                  LogStr, [append]),

  {NewFps, NewFramerate, NewTime} = case (Now - LastFrameTime)/1000000 > 1000 of
                   true -> {0, Fps, Now};
                   false -> {Fps+1, Framerate, LastFrameTime}
                 end,
  State#{ frame_count => NewFps
        , framerate => NewFramerate
        , last_frame_time => NewTime
        , write_offset => NewOffset
        };


%% Render events
% dispatch_event(_, {_, {{ts, _}, {kind, notice}, {data, render_queued}}}, _) ->
%   ok;
% dispatch_event({T0, T1, Size}, {Ts, {{ts, HappenedAt}, {kind, command}, {command, request_frame}}}, State) when ((T1 - Ts)/1000000) < 3 ->
%   Frame = chalk_pipeline:flush(),
%   T2 = erlang:system_time(),
%   {MsgSize, T3, T4} = do_render(Frame, State),
%   %io:format("\n\nrust:event_dispatching:\t~pms\n", [(Ts - HappenedAt)/1000000]),
%   %io:format("erlang:port_receive:\t~pms\n", [(T0-Ts)/1000000]),
%   %{_, MsgCount} = process_info(whereis(chalk_port), message_queue_len),
%   %io:format("port:message_queue_len:\t~p\n", [MsgCount]),
% 
%   %io:format("erlang:msg_byte_size:\t~p bytes\n", [Size]),
%   %io:format("erlang:binary_to_term:\t~pms\n", [(T1-T0)/1000000]),
%   %io:format("chalk_pipeline:flush/0:\t~pms\n", [(T2 - T1)/1000000]),
%   %io:format("erlang:term_to_binary:\t~pms\n", [(T3-T2)/1000000]),
%   %io:format("erlang:msg_byte_size:\t~p bytes\n", [MsgSize]),
%   %io:format("erlang:port_command/1:\t~pms\n", [(T4 - T3)/1000000]),
%   ok;
% 
% %% Droppable events
% dispatch_event(_, {_,{{ts, _}, {kind, relay}, {data, device_event}}}, _) ->
%   ok;
% 
% %% Relay Events
% dispatch_event({T0,T1, Size}, {Ts, {{ts, HappenedAt}, {kind, relay}, {data, E}}}, State) ->
%   ok = chalk_event_server:send({Ts, E}),
%   T2 = erlang:system_time(),
%   %io:format("\n\nrust:event_dispatching:\t~pms\n", [(Ts - HappenedAt)/1000000]),
%   %io:format("erlang:msg_byte_size:\t~p bytes\n", [Size]),
%   %io:format("erlang:port_receive:\t~pms\n", [(T0-Ts)/1000000]),
%   %io:format("erlang:binary_to_term:\t~pms\n", [(T1-T0)/1000000]),
%   %io:format("chalk_port:do_fw_evt/2:\t~pms\n", [(T2 - T1)/1000000]),
%   ok;
% 
%% Ignore the rest
dispatch_event(_, _, State) -> State.

% 
% 
% do_render(#{ port := Port }) ->
%   Frame = chalk_pipeline:flush(),
%   do_command(Port, render, Frame).
% do_render(Frame, #{ port := Port }) ->
%   do_command(Port, render, Frame).

do_command(Writer, Offset, Kind, Data) ->
  Cmd = {{ref,0}, {kind,Kind}, {data,Data}},
  {ok, NewOffset} = memmap:write_term(Writer, Cmd, Offset),
  T0 = erlang:system_time(),
  {NewOffset, T0}.
