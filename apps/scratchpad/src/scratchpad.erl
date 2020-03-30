-module(scratchpad).

-export([ reload/0
        , start/0
        , stop/0
        ]).

start() -> application:ensure_all_started(scratchpad).

stop() -> application:stop(scratchpad).

reload() ->
  stop(),
  start().
