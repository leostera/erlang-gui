-module(canvas_server).

-behavior(gen_server).

-export([ start_link/1
        , stop/0
        , init/1
        , terminate/2
        , handle_call/3
        ]).

-export([ do_flush/1
        , do_draw/1
        ]).

-export([ flush/0
        , set_frame_rate/1
        , reset_frame_count/0
        ]).

%%===============================================================================
%% Behavior Callbacks
%%===============================================================================

initial_state() ->
  #{ frame_timer => none
   , frame_count => 0
   }.

init(_Args) -> {ok, initial_state()}.

terminate(_, _State) ->
  ok.

handle_call(flush, _From, #{ frame_count := FC }=State) ->
  {T0, _} = timer:tc(canvas_server, do_flush, [State]),
  io:format("canvas_server:do_flush/1: ~p\n", [T0]),
  {reply, ok, State#{ frame_count => FC+1 }};

handle_call({set_frame_rate, Rate}, _From, #{ frame_timer := TRef }=State) ->
  {ok, Timer} = do_set_frame_rate(TRef, Rate),
  {reply, ok, State#{ frame_timer => Timer }};

handle_call(reset_frame_count, _From, State) ->
  {reply, ok, State#{ frame_count => 0 }};

handle_call(_, _From, State) ->
  {noreply, State}.


%%===============================================================================
%% Api
%%===============================================================================

start_link(_Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, _Args, []).

stop() ->
  gen_server:stop(?MODULE).

flush() -> gen_server:call(?MODULE, flush).

set_frame_rate(Rate) when is_float(Rate) -> gen_server:call(?MODULE, {set_frame_rate, Rate}).

reset_frame_count() -> gen_server:call(?MODULE, reset_frame_count).

%%===============================================================================
%% Internal
%%===============================================================================

do_draw(0) -> ok;
do_draw(_) ->
  Canvas = sk_canvas:new(1000, 1000),
  sk_canvas:draw_color(Canvas, sk_color:cyan()),
  frame_buffer_server:foreach(
    fun (none) -> ok;
        ({{X, Y}, Frame}) ->
        sk_canvas:translate(Canvas, X, Y),
        sk_canvas:draw_picture(Canvas, Frame),
        sk_canvas:translate(Canvas, 0.0, 0.0)
    end),
  Picture = sk_picture:from_canvas(Canvas),
  Bytes = sk_picture:as_bytes(Picture),
  chalk:render(Bytes).

do_flush(_State) ->
  #{ frame_count := Size } = frame_buffer_server:statistics(),
  {T0, _} = timer:tc(canvas_server, do_draw, [Size]),
  io:format("canvas_server:do_draw/1: ~p\n", [T0]),
  ok.

do_set_frame_rate(none, Rate) ->
  timer:apply_interval( round(1000 / Rate), ?MODULE, flush, []);
do_set_frame_rate(TRef, Rate) ->
  timer:cancel(TRef),
  do_set_frame_rate(none, Rate).
