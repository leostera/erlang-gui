-module(chalk_node_tree).

-compile([export_all]).

new() -> setup().

default_table_opts() ->
  [ named_table
  , ordered_set
  , {read_concurrency, true}
  , {write_concurrency, true}
  , public
  ].

setup() ->
  Opts = default_table_opts(),
  Cache = cets:new(2, <<"chalk_render_tree_cache">>, Opts),
  Refs = cets:new(16, <<"chalk_render_tree_ref_table">>, Opts),
  Zs = ets:new(chalk_render_tree_flush_table, Opts),
  #{ refs => Refs
   , cache => Cache
   , render => Zs
   }.

clear(#{ refs := RefTable
       , cache := Cache
       , render := ZsTable }) ->
  cets:delete_all_objects(RefTable),
  cets:delete_all_objects(Cache),
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

fold(F, #{ render := ZsTable, refs := RefTable, cache := Cache }) ->
  cets:foldl(fun
               ({Ref, Fn}, _) ->
                 (catch (case Fn() of
                           cached ->
                             [{_, {LastPos, LastPic}}] = cets:lookup(Cache, Ref),
                             ets:insert(ZsTable, {key(LastPos, Ref), LastPic});

                           {new_frame, NewPos, NewPic} ->
                             KV = {key(NewPos, Ref), NewPic},
                             cets:insert(Cache, {Ref, {NewPos, NewPic}}),
                             ets:insert(ZsTable, KV),
                             cets:insert(RefTable, {Ref, Fn});

                           _ -> ok
                         end))
            end, none, RefTable),
  ets:foldl(fun ({K,V}, Acc) -> F({unkey(K),V}), Acc end, none, ZsTable),
  ets:delete_all_objects(ZsTable),
  ok.

id() -> make_ref().

key({X,Y,Z}, Ref) -> {Z, Y, X, Ref}.
unkey({Z,Y,X,_}) -> {X,Y,Z}.

size(#{ refs := RefTable }) -> ets:info(RefTable, size).
