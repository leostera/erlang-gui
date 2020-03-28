-module(chalk).

-export([ render/2
        , render/3
        , reload/0
        , start/0
        , stop/0
        , set_frame_rate/1
        ]).

start() -> application:ensure_all_started(chalk).

stop() -> application:stop(chalk).

reload() ->
  stop(),
  start().

set_frame_rate(X) -> chalk_pipeline:set_frame_rate(X).

render({X, Y}, Picture) when is_float(X) and is_float(Y) ->
  render({X, Y, 0.0}, Picture);
render({X, Y, Z}, Picture) when is_float(X) and is_float(Y) and is_float(Z) ->
  {ok, Ref} = chalk_pipeline:draw({{X,Y,Z}, Picture}),
  {ok, Ref}.

render(Ref, {X, Y, Z}, Picture) when is_float(X) and is_float(Y) and is_float(Z) ->
  ok = chalk_pipeline:draw({Ref, {X,Y,Z}, Picture}),
  ok.
