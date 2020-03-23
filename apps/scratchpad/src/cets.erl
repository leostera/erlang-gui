-module(cets).

-compile([export_all]).

info(#{ tables := Ts }, size) ->
  maps:fold(fun (_, T, Acc) -> Acc + ets:info(T, size) end, 0, Ts).

new(Partitions, Prefix, Opts) ->
  Tables = lists:foldl(
             fun (Idx, Acc) ->
                 Name = <<(Prefix)/binary, "_", (integer_to_binary(Idx))/binary>>,
                 Atom = binary_to_atom(Name, utf8),
                 Table = ets:new(Atom, Opts),
                 maps:put(Idx, Table, Acc)
             end, #{}, lists:seq(0, Partitions)),
  #{ tables => Tables, partitions => Partitions }.

lookup(#{ tables := Ts }, Key) ->
  lists:foldl(fun (T, []) -> ets:lookup(T, Key);
                  (_, Acc) -> Acc
              end, [], Ts).

insert(#{ tables := Ts, partitions := Partitions }, KV) ->
  T = random_table(Ts, Partitions),
  ets:insert(T, KV).

foldl(Fn, _, #{ tables := Ts }) ->
  Self = self(),
  Links = [ spawn_link(fun () ->
                           ets:foldl(Fn, none, T),
                           Self ! done
                       end) || {_, T} <- maps:to_list(Ts) ],
  LinkCount = length(Links),
  await_results(LinkCount).

await_results(0) -> ok;
await_results(C) -> receive _ -> await_results(C-1) end.


random_table(Ts, Partitions) ->
  Idx = rand:uniform(Partitions),
  maps:get(Idx, Ts).
