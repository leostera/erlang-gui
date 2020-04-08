-module(chalk_render_pipeline).

-behaviour(gen_server).

-export([ start_link/2
        , stop/0
        , init/1
        , terminate/2
        , handle_call/3
        , handle_cast/2
        ]).

-export([ flush/0
        , dump/0
        , set_viewport_size/1
        , make_frame/2
        ]).

%%==============================================================================
%% Behavior callbacks
%%==============================================================================

init(_Args) ->
  process_flag(priority, high),
  {ok, initial_state()}.

terminate(_, _) -> ok.

handle_call(flush, _From, State) -> do_flush(State);
handle_call({set_viewport_size, Size}, _From, State) -> do_set_viewport_size(Size, State);
handle_call(dump, _From, State) -> {reply, State, State};
handle_call(_, _From, State) -> {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.


%%==============================================================================
%% Api
%%==============================================================================

start_link(Args, Opts) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

stop() -> gen_server:stop(?MODULE).

flush() -> gen_server:call(?MODULE, flush).

dump() -> gen_server:call(?MODULE, dump).

set_viewport_size(Size={W, H}) when is_integer(W) and is_integer(H)
                                and (W > 0) and (H > 0) ->
  gen_server:call(?MODULE, {set_viewport_size, Size}).


%%==============================================================================
%% Internal
%%==============================================================================

initial_state() ->
  #{ node_tree => gen_server:call(chalk_node_tree, dump, infinity)
   , size => {1920, 1080}
   }.

do_set_viewport_size(Size, State) -> {reply, ok, State#{ size => Size }}.

do_flush(#{ size := {W, H}, node_tree := NodeTree }=State) ->
  {T, Frame} = timer:tc(fun () -> chalk_render_pipeline:make_frame(NodeTree, {W, H}) end),
  io:format("make_frame: ~pμs\n\n\n", [T]),
  {reply, {ok, Frame}, State}.

make_frame(NodeTree, {W, H}) ->
  Canvas = sk_canvas:new(W, H),
  sk_canvas:clip_rect(Canvas, W, H),
  % Fold node tree into the current screen
  chalk_node_tree:foreach(
     fun (Pic) -> sk_canvas:draw_picture_at(Canvas, {0.0,0.0}, Pic) end
   , NodeTree
   ),
  Picture = sk_picture:from_canvas(Canvas),
  sk_picture:as_bytes(Picture).
