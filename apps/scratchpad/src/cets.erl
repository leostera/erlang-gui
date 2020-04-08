-module(cets).

-compile([export_all]).

info(#{ tables := Ts }, size) ->
  maps:fold(fun (_, T, Acc) -> Acc + ets:info(T, size) end, 0, Ts);
info(#{ tables := Ts }, sizes) ->
  maps:fold(fun (_, T, Acc) -> Acc#{ T => ets:info(T, size) } end, #{}, Ts).

new(Prefix) -> new(default_partition_count(), Prefix).

new(Partitions, Prefix) -> new(Partitions, Prefix, default_table_options()).

default_table_options() ->
  [ named_table
  , ordered_set
  , {read_concurrency, true}
  , {write_concurrency, true}
  , public
  ].

default_partition_count() ->
  Schedulers = erlang:system_info(schedulers_online),
  Procs = erlang:system_info(logical_processors_available),
  case Schedulers > Procs of
    true -> round(Procs / 2.0);
    false -> round(Schedulers / 2.0)
  end.

new(Partitions, Prefix, Opts) ->
  Tables = lists:foldl(
             fun (Idx, Acc) ->
                 Name = <<(Prefix)/binary, "_", (integer_to_binary(Idx))/binary>>,
                 Atom = binary_to_atom(Name, utf8),
                 Table = ets:new(Atom, Opts),
                 maps:put(Idx, Table, Acc)
             end, #{}, lists:seq(0, Partitions-1)),
  #{ tables => Tables
   , partitioning_fn => fun (_Key, Ps) -> rand:uniform(maps:size(Ps)-1) end
   , partitions => Partitions }.

set_partitioning_function(Fn, S) ->
  S#{ partitioning_fn => Fn }.

delete(#{ tables := Ts }, Key) ->
  [ ets:delete(T, Key) || T <- maps:values(Ts) ],
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

insert(S=#{ tables := Ts
          , partitioning_fn := Fn
       }, {Key, Value}) ->
  Table = case partition_residence(S, Key) of
            {ok, T} -> T;
            none -> pick_table(Fn, Key, Ts)
          end,
  ets:insert(Table, {Key, Value}).

fold(Fn, AccFn, #{ tables := Ts }) ->
  Self = self(),
  Links = [ spawn(fun () ->
                      case (catch ets:foldl(Fn, AccFn(), T)) of
                        {'EXIT', Reason} -> io:format("Folding error: ~p\n", [Reason]);
                        Result -> Self ! {done, {K, Result}}
                      end
                  end) || {K, T} <- maps:to_list(Ts) ],
  LinkCount = length(Links),
  await_results(LinkCount, []).

foldl(Fn, Acc, #{ tables := Ts }) ->
  Self = self(),
  Links = [ spawn(fun () ->
                      case (catch ets:foldl(Fn, Acc, T)) of
                        {'EXIT', Reason} -> io:format("Folding error: ~p\n", [Reason]);
                        Result -> Self ! {done,  Result}
                      end
                  end) || T <- maps:values(Ts) ],
  LinkCount = length(Links),
  await_results(LinkCount, []).

await_results(0, Acc) -> Acc;
await_results(C, Acc) -> receive {done, Res} -> await_results(C-1, [Res|Acc]) end.

pick_table(Fn, Key, Ts) ->
  Idx = Fn(Key, Ts),
  maps:get(Idx, Ts).
