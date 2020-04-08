-module(chalk_node_tree).

-behaviour(gen_server).

-export([ start_link/2
        , stop/0
        , init/1
        , terminate/2
        , handle_call/3
        , handle_cast/2
        ]).

-export([ clear/0
        , dump/0
        , get/1
        , add/1
        , foreach/1
        , foreach/2
        , size/0
        , rect/1
        , rect/2
        ]).


%%==============================================================================
%% Behavior callbacks
%%==============================================================================

init(_Args) ->
  process_flag(priority, high),
  {ok, initial_state()}.

terminate(_, _) -> ok.

handle_call({foreach, Fn}, _From, State) -> do_foreach(Fn, State);
handle_call({add, Node}, _From, State) -> do_add(Node, State);
handle_call({get, Key}, _From, State) -> do_get(Key, State);
handle_call({get_rect, Node}, _From, State) -> do_get_rect(Node, State);
handle_call(size, _From, State) -> do_size(State);
handle_call(clear, _From, State) -> do_clear(State);
handle_call(dump, _From, State) -> {reply, State, State};
handle_call(_, _From, State) -> {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.


%%==============================================================================
%% Api
%%==============================================================================

start_link(Args, Opts) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

stop() -> gen_server:stop(?MODULE).

clear() -> gen_server:call(?MODULE, clear).

dump() -> gen_server:call(?MODULE, dump).

get(Key) -> gen_server:call(?MODULE, {get, Key}).

rect(Node) -> gen_server:call(?MODULE, {get_rect, Node}).

rect(Node, State) ->
  {reply, Rect, _} = do_get_rect(Node, State),
  Rect.

add(Node) when is_function(Node) -> gen_server:call(?MODULE, {add, Node}).

foreach(Fn) when is_function(Fn) -> gen_server:call(?MODULE, {foreach, Fn}).

foreach(Fn, State) when is_function(Fn) ->
  {reply, ok, _} = do_foreach(Fn, State),
  ok.

size() -> gen_server:call(?MODULE, size).

%%==============================================================================
%% Internal
%%==============================================================================

initial_state() ->
  Dimensions = {500, 1920, 1080},
  Cache = cets:new(<<"chalk_render_tree_cache">>),
  Refs = cets:new(<<"chalk_render_tree_ref_table">>),
  % Try out the octree
  Octree = eu_octree:new({500, 1920, 1080}),
  Z0 = cets:new(<<"chalk_render_tree_flush_table">>),
  Zs = cets:set_partitioning_function(
         fun ({Z, Y, X, _}, Partitions) ->
             Point = {Z,Y,X},
             case eu_octree:member(Point, Octree) of
               {hit, {Idx0, Idx1}}  ->
                 PartSize = maps:size(Partitions),
                 round(math:fmod((Idx0+1 * Idx1+1),PartSize));
               _ -> 0
             end
         end, Z0),
  #{ refs => Refs
   , cache => Cache
   , render => Zs
   , octree => Octree
   , dimensions => Dimensions
   }.


do_clear(#{ refs := RefTable
          , cache := Cache
          , render := ZsTable }) ->
  cets:delete_all_objects(RefTable),
  cets:delete_all_objects(Cache),
  cets:delete_all_objects(ZsTable),
  ok.

do_get(Ref, #{ refs := RefTable }=State) ->
  Result = case cets:lookup(RefTable, Ref) of
    [] -> none;
    [V] -> V
  end,
  {reply, Result, State}.

do_get_rect(Ref, #{ cache := Cache }=State) ->
  case cets:lookup(Cache, Ref) of
    [] ->
      {reply, {error, rect_is_yet_unknown}, State};
    [{Ref, {{X, Y, _}, {W, H}, _Pict}}] ->
      {reply, eu_rect:make(X, Y, W, H), State}
  end.


do_add(Fn, #{ refs := RefTable }=State) ->
  Ref = id(),
  true = cets:insert(RefTable, {Ref, Fn}),
  {reply, {ok, Ref}, State}.

do_foreach(F, #{ render := ZsTable
               , refs := RefTable
               , octree := Octree
               , cache := Cache }=State) ->
  {T, _} = timer:tc(fun () ->
  cets:foldl(
    fun ({Ref, Fn}, _) ->
        case (catch Fn()) of
          cached -> queue_cached_frame(Ref, Cache, ZsTable);
          {new_frame, _, _, _}=Frame -> queue_new_frame(Ref, Frame, State);
          _ -> ok
          end
    end, ok, RefTable)
  end),
  io:format("build z table: ~pμs\n", [T]),
  {T1, Pictures} =
    timer:tc(fun () ->
      cets:fold(fun ({{Z, Y, X, _}, Pic}, {_, C}) ->
                     sk_canvas:draw_picture_at(C, {X, Y}, Pic),
                     {{Z, Y, X}, C}
                end,
                fun () -> {first, sk_canvas:new(1920, 1080)} end,
                ZsTable)
    end),
  io:format("z octants draw: ~pμs\n", [T1]),
  Frame = lists:foldl(fun ({I, {Pos, TempC}}, C) ->
                          Idx = eu_octree:linear_idx_to_pair(I),
                          {_, {X,_}, {Y,_}} = eu_octree:limits(Idx, Octree),
                          Pic = sk_picture:from_canvas(TempC),
                          sk_canvas:draw_picture_at(C, {X*1.0, Y*1.0}, Pic),
                          C;
                          (_, C) -> C
                      end,
                      sk_canvas:new(1920, 1080),
                      lists:sort(fun ({A, _}, {B, _}) -> A < B end, Pictures)),
  F(sk_picture:from_canvas(Frame)),
  cets:delete_all_objects(ZsTable),
  {reply, ok, State}.

queue_cached_frame(Ref, Cache, ZsTable) ->
  [{_, {LastPos, _LastDims, LastPic}}] = cets:lookup(Cache, Ref),
  cets:insert(ZsTable, {key(LastPos, Ref), LastPic}).

queue_new_frame( Ref
               , {new_frame, {X, Y, Z}=NewPos, {W, H}, NewPic}
               , #{ render := ZsTable, cache := Cache, octree := Oct }) ->
  case eu_octree:member({Z, Y, X}, Oct) of
    {hit, _} -> NewDims = { W*1.0, H*1.0 },
                cets:insert(Cache, {Ref, {NewPos, NewDims, NewPic}}),
                cets:insert(ZsTable, {key(NewPos, Ref), NewPic});
    _ -> ok
  end.



do_size(#{ refs := RefTable }=State) ->
  {reply, {ok, ets:info(RefTable, size)}, State}.

%%==============================================================================
%% Utilities
%%==============================================================================

id() -> make_ref().
key({X,Y,Z}, _Ref) -> {Z, Y, X, erlang:system_time()}.
