-module(chalk_pipeline).

-behaviour(gen_server).

-export([ start_link/2
        , stop/0
        , init/1
        , terminate/2
        , handle_call/3
        , handle_cast/2
        ]).

-export([ draw/1
        , flush/0
        , clear/0
        , set_frame_rate/1
        , set_viewport_size/1
        , make_frame/2
        ]).

%%==============================================================================
%% Behavior callbacks
%%==============================================================================

initial_state() ->
  #{ nodes => chalk_node_tree:new()
   , size => {3840, 2160}
   , frame_timer => none
   }.

init(_Args) -> {ok, initial_state()}.

terminate(_, _) -> ok.

handle_call({draw_update, DrawReq}, _From, State) ->
  {ok, NewState} = do_draw_update(DrawReq, State),
  {reply, ok, NewState};

handle_call({draw_new, DrawReq}, _From, State) ->
  {ok, Ref, NewState} = do_draw_new(DrawReq, State),
  {reply, {ok, Ref}, NewState};

handle_call(flush, _From, State) ->
  {ok, NewState} = do_flush(State),
  {reply, ok, NewState};

handle_call(clear, _From, State) ->
  ok = do_clear(State),
  {reply, ok, State};

handle_call({set_frame_rate, Rate}, _From, #{ frame_timer := TRef }=State) ->
  {ok, Timer} = do_set_frame_rate(TRef, Rate),
  {reply, ok, State#{ frame_timer => Timer }};

handle_call({set_viewport_size, Size}, _From, State) ->
  {reply, ok, State#{ size => Size }}.

handle_cast(_Msg, State) -> {noreply, State}.


%%==============================================================================
%% Api
%%==============================================================================

start_link(Args, Opts) -> gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

stop() -> gen_server:stop(?MODULE).

clear() -> gen_server:call(?MODULE, clear).

flush() -> gen_server:call(?MODULE, flush).

draw({{X,Y,Z}, _Pic}=Canvas) when is_float(X) and is_float(Y) and is_float(Z)->
  {ok, Ref} = gen_server:call(?MODULE, {draw_new, Canvas}),
  {ok, Ref};

draw({_Ref, {X,Y,Z}, _Pic}=DrawReq) when is_float(X) and is_float(Y) and is_float(Z)->
  ok = gen_server:call(?MODULE, {draw_update, DrawReq}),
  ok.

set_frame_rate(Rate) when is_float(Rate) ->
  gen_server:call(?MODULE, {set_frame_rate, Rate}).

set_viewport_size(Size={W, H}) when is_integer(W) and is_integer(H)
                                and (W > 0) and (H > 0) ->
  gen_server:call(?MODULE, {set_viewport_size, Size}).


%%==============================================================================
%% Internal
%%==============================================================================

do_draw_update(DrawReq, #{ nodes := Nodes }=State) ->
  ok = chalk_node_tree:update(DrawReq, Nodes),
  {ok, State}.

do_draw_new(DrawReq, #{ nodes := Nodes }=State) ->
  {ok, Ref} = chalk_node_tree:add(DrawReq, Nodes),
  {ok, Ref, State}.

do_clear(#{ nodes := Nodes }=State) ->
  chalk_node_tree:clear(Nodes).

do_flush(#{ nodes := Nodes, size := {W, H} }=State) ->
  case chalk_node_tree:status(Nodes) of
    dirty ->
      io:format("flushing!\n", []),
      Frame = chalk_pipeline:make_frame(Nodes, {W, H}),
      ok = chalk_port:render(Frame);
    clean ->
      ok
  end,
  {ok, State}.

do_set_frame_rate(none, Rate) ->
  timer:apply_interval( round(1000 / Rate), ?MODULE, flush, []);
do_set_frame_rate(TRef, Rate) ->
  timer:cancel(TRef),
  do_set_frame_rate(none, Rate).

make_frame(Nodes, {W, H}) ->
  Canvas = sk_canvas:new(W, H),
  sk_canvas:clip_rect(Canvas, W, H),
  chalk_node_tree:fold(
    fun ({{X, Y, _Z}, Picture}) ->
        sk_canvas:translate(Canvas, X, Y),
        sk_canvas:draw_picture(Canvas, Picture),
        sk_canvas:translate(Canvas, -X, -Y)
    end, Nodes),
  Picture = sk_picture:from_canvas(Canvas),
  sk_picture:as_bytes(Picture).
