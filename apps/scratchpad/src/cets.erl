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

delete(#{ tables := Ts }, Key) ->
  [ (catch ets:delete(T, Key)) || T <- maps:values(Ts) ],
  ok.

delete_all_objects(#{ tables := Ts }) ->
  [ ets:delete_all_objects(T) || T <- maps:values(Ts) ],
  ok.

lookup(#{ tables := Ts }, Key) ->
  lists:foldl(fun (T, []) -> ets:lookup(T, Key);
                  (_, Acc) -> Acc
              end, [], maps:values(Ts)).

partition_residence(#{ tables := Ts }, Key) ->
  lists:foldl(fun (T, none) ->
                    case ets:lookup(T, Key) of
                      [] -> none;
                      _ -> {ok, T}
                    end;
                  (_, Acc) -> Acc
              end, none, maps:values(Ts)).

insert(S=#{ tables := Ts, partitions := Partitions }, {Key, Value}) ->
  Table = case partition_residence(S, Key) of
            {ok, T} -> T;
            none -> random_table(Ts, Partitions)
          end,
  ets:insert(Table, {Key, Value}).

foldl(Fn, _, #{ tables := Ts }) ->
  Self = self(),
  Links = [ spawn_link(fun () ->
                           process_flag(priority, high),
                           ets:foldl(Fn, none, T),
                           Self ! done,
                           process_flag(priority, normal)
                       end) || T <- maps:values(Ts) ],
  LinkCount = length(Links),
  await_results(LinkCount, []).

await_results(0, Acc) -> Acc;
await_results(C, Acc) -> receive Res -> await_results(C-1, [Res|Acc]) end.


random_table(Ts, Partitions) ->
  Idx = rand:uniform(Partitions),
  maps:get(Idx, Ts).
