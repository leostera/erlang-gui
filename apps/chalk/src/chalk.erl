-module(chalk).

-export([ render/1
        , start/0
        , stop/0
        ]).

start() ->
  chalk_event_server:start_link([]),
  chalk_port:start_link([]),
  chalk_frame_dispatcher:start_link([]),
  chalk_pipeline:start_link([]),
  frame_buffer_server:start_link([]),
  canvas_server:start_link([]),
  canvas_server:set_frame_rate(20.0).

stop() ->
  chalk_event_server:stop(),
  chalk_port:stop(),
  chalk_frame_dispatcher:stop(),
  chalk_pipeline:stop().

render(Data) when is_list(Data) ->
  render(erlang:list_to_binary(Data));
render(Data) when is_binary(Data) ->
  ok = chalk_pipeline:queue(Data),
  ok.
