-module(chalk).

-export([ render/1
        , start/0
        ]).

start() ->
  chalk_port:start_link([]),
  chalk_frame_dispatcher:start_link([]),
  chalk_pipeline:start_link([]).

render(Data) when is_list(Data) ->
  render(erlang:list_to_binary(Data));
render(Data) when is_binary(Data) ->
  ok = chalk_pipeline:queue(Data),
  ok.
