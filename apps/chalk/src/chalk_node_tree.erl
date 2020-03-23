-module(chalk_node_tree).

-compile([export_all]).

new() -> setup().

setup() ->
  Refs = cets:new(16, <<"chalk_default_node_tree_update_table">>, [named_table, ordered_set, {read_concurrency, true}, {write_concurrency, true}, public]),
  Zs = ets:new(chalk_default_node_tree_render_table, [named_table, ordered_set, {read_concurrency, true}, {write_concurrency, true}, public]),
  #{ refs => Refs, render => Zs }.

clear(#{ refs := RefTable, render := ZsTable }) ->
  ets:delete_all_objects(RefTable),
  ets:delete_all_objects(ZsTable),
  ok.

get(Ref, #{ refs := RefTable }) ->
  case cets:lookup(RefTable, Ref) of
    [] -> none;
    [V] -> V
  end.

add(Fn, #{ refs := RefTable }) ->
  Ref = id(),
  true = cets:insert(RefTable, {Ref, Fn}),
  {ok, Ref}.

fold(F, #{ render := ZsTable, refs := RefTable }) ->
  cets:foldl(fun ({Ref, Fn}, _) ->
                {ok, {X,Y,Z}, Pic} = Fn(),
                KV = {{Z,Y,X,Ref}, Pic},
                ets:insert(ZsTable, KV)
            end, none, RefTable),
  ets:foldl(fun ({K,V}, Acc) -> F({unkey(K),V}), Acc end, none, ZsTable),
  ets:delete_all_objects(ZsTable),
  ok.

id() -> make_ref().

key({X,Y,Z}, Ref) -> {Z, Y, X, Ref}.
unkey({Z,Y,X,_}) -> {X,Y,Z}.

size(#{ refs := RefTable }) -> ets:info(RefTable, size).
