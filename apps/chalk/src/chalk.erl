-module(chalk).

-export([ start/0
        , stop/0
        , reload/0
        ]).

-export([ add_node/1
        , new_frame/3
        ]).

start() -> application:ensure_all_started(chalk).

stop() -> application:stop(chalk).

reload() ->
  stop(),
  start().

add_node(Fn) -> chalk_node_tree:add(Fn).

new_frame(Pos={X, Y, Z}, Dim={W, H}, Pict)
  when is_number(X) and is_number(Y) and is_number(Z)
   and is_number(W) and is_number(H)
   and is_reference(Pict) ->
  {new_frame, Pos, Dim, Pict}.

