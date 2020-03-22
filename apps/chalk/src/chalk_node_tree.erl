-module(chalk_node_tree).

-compile([export_all]).

new() ->
  Tables = setup(),
  mark_clean(Tables),
  Tables.

setup() ->
  Config = ets:new(chalk_default_node_tree_config_table, [named_table, ordered_set]),
  Refs = ets:new(chalk_default_node_tree_update_table, [named_table, ordered_set]),
  Zs = ets:new(chalk_default_node_tree_render_table, [named_table, ordered_set]),
  #{ refs => Refs, render => Zs, config => Config}.

mark_empty(#{ config := C}) ->
  ets:insert(C, {'$empty', true}).

mark_clean(#{ config := C }) ->
  ets:insert(C, {'$status', clean}).

mark_dirty(#{ config := C }) ->
  ets:insert(C, {'$status', dirty}).

status(#{ config := C }) ->
  case ets:lookup(C, '$status') of
    [] -> clean;
    [{'$status', V}] -> V
  end.

clear(#{ refs := RefTable, render := ZsTable }=Tables) ->
  ets:delete_all_objects(RefTable),
  ets:delete_all_objects(ZsTable),
  mark_clean(Tables),
  ok.

get(Ref, #{ refs := RefTable }) ->
  case ets:lookup(RefTable, Ref) of
    [] -> none;
    [V] -> V
  end.

update({Ref, Pos, Value}, #{ refs := RefTable, render := ZsTable }=Tables) ->
  % Find old position
  {Ref, {OldPos, _}} = get(Ref, Tables),

  % Remove old position from render table
  OldKey = key(OldPos, Ref),
  true = ets:delete(ZsTable, OldKey),

  % Insert new position into render table
  true = ets:insert(ZsTable, {key(Pos, Ref), Value}),
  % Update
  true = ets:insert(RefTable, {Ref, {Pos, Value}}),

  mark_dirty(Tables),

  ok.

add({Pos, Value}, #{ refs := RefTable, render := ZsTable}=Tables) ->
  Ref = id(),
  true = ets:insert(RefTable, {Ref, {Pos, Value}}),
  true = ets:insert(ZsTable, {key(Pos, Ref), Value}),
  mark_dirty(Tables),
  {ok, Ref}.

fold(F, #{ render := ZsTable }=Tables) ->
  ets:foldl(fun ({K,V}, Acc) -> F({unkey(K),V}), Acc end, none, ZsTable),
  mark_clean(Tables).

id() -> make_ref().

key({X,Y,Z}, Ref) -> {Z, Y, X, Ref}.
unkey({Z,Y,X,_}) -> {X,Y,Z}.

size(#{ refs := RefTable }) -> ets:info(RefTable, size).
