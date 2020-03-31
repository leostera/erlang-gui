-module(chalk_pipeline).

-behaviour(gen_server).

-export([ start_link/2
        , stop/0
        , init/1
        , terminate/2
        , handle_call/3
        , handle_cast/2
        ]).

-export([ register/1
        , flush/0
        , flush_many/0
        , clear/0
        , dump/0
        , set_viewport_size/1
        , make_frame/2
        ]).

%%==============================================================================
%% Behavior callbacks
%%==============================================================================

initial_state() ->
  #{ nodes => chalk_node_tree:new()
   , size => {1920, 1080}
   }.

init(_Args) ->
  process_flag(priority, high),
  {ok, initial_state()}.

terminate(_, _) -> ok.

handle_call(dump, _From, State) -> {reply, State, State};

handle_call({register, Fn}, _From, State) ->
  {ok, Ref, NewState} = do_register(Fn, State),
  {reply, {ok, Ref}, NewState};

handle_call(flush, _From, State) ->
  {ok, T, NewState} = do_flush(State),
  {reply, T, NewState};

handle_call(clear, _From, State) ->
  ok = do_clear(State),
  {reply, ok, State};

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

flush_many() -> lists:foreach(fun chalk_pipeline:flush/0, lists:seq(0, 1000)).

register(F) -> gen_server:call(?MODULE, {register, F}).

dump() -> gen_server:call(?MODULE, dump).

set_viewport_size(Size={W, H}) when is_integer(W) and is_integer(H)
                                and (W > 0) and (H > 0) ->
  gen_server:call(?MODULE, {set_viewport_size, Size}).


%%==============================================================================
%% Internal
%%==============================================================================

do_register(Fn, #{ nodes := Nodes }=State) ->
  {ok, Ref} = chalk_node_tree:add(Fn, Nodes),
  {ok, Ref, State}.

do_clear(#{ nodes := Nodes }) ->
  chalk_node_tree:clear(Nodes).

do_flush(#{ nodes := Nodes, size := {W, H} }=State) ->
  Frame = chalk_pipeline:make_frame(Nodes, {W, H}),
  {ok, Frame, State}.

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
