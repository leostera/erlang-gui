-module(obj).

-behaviour(gen_server).

-export([ start_link/1
        , init/1
        , terminate/2
        , handle_cast/2
        , handle_call/3
        , allow_move/0

        , start_n/1
        , draw/1
        ]).

draw(O) -> gen_server:call(O, draw).

start_n(N) ->
  [
    start_link([])
    || _ <- lists:seq(0, N)
  ].

start_link(_Args) -> gen_server:start_link(?MODULE, _Args, []).

init(_Args) ->
  chalk_event_server:register(self()),
  {ok, FBRef} = frame_buffer_server:register(),
  {ok, MoveTimer} = timer:apply_interval(20, ?MODULE, allow_move, []),
  {ok, DrawTimer} = timer:apply_interval(100, ?MODULE, draw, [self()]),
  State = #{ state => free
           , pos => {200,200}
           , can_move_timer => MoveTimer
           , draw_timer => DrawTimer
           , max_screen => {1, 1}
           , frame_buffer_ref => FBRef
           },
  {ok, State}.

allow_move() -> gen_server:cast(?MODULE, ready_to_move).

terminate(_, _) -> ok.

handle_cast(ready_to_move, State) ->
  {noreply, State#{ can_move => true }};

handle_cast({{type, resized}, Data}, State) ->
  io:format("~p\n", [Data]),
  #{ w := W, h := H } = maps:from_list(Data),
  {noreply, State#{ max_screen => {W, H} }};

handle_cast({{type, mouse_input}, Data}, State) ->
  io:format("~p\n", [Data]),
  handle_mouse_input(maps:from_list(Data), State);

handle_cast({{type, cursor_moved}, Data}, #{ can_move := true }=State) ->
  io:format("~p\n", [Data]),
  #{ x := X, y := Y } = maps:from_list(Data),
  handle_cursor_moved({X,Y}, State);


handle_cast(_E, State) ->
  {noreply, State}.


handle_call(draw, _From, State) ->
  {reply, do_draw(State), State};


handle_call(_, _, S) -> {noreply, S}.


handle_mouse_input(#{ state := pressed }, State) ->
  State2 = State#{ state => grabbed },
  {noreply, State2};
handle_mouse_input(#{ state := released }, State) ->
  State2 = State#{ state => free },
  {noreply, State2}.

handle_cursor_moved(_, #{ state := free }=State) ->
  {noreply, State};
handle_cursor_moved({X,Y}, #{ state := grabbed }=State) ->
  State2 = State#{ pos => {X, Y}, can_move => false },
  {noreply, State2}.


do_draw(#{ pos := {X,Y}, state := S,  frame_buffer_ref := Ref }) ->
  Canvas = sk_canvas:new(100, 100),
  sk_canvas:draw_color(Canvas, sk_color:cyan()),
  Paint = sk_paint:new(),
  sk_paint:set_stroke_width(Paint, 4.0),
  sk_paint:set_color(Paint, pick_color(S)),
  Rect = make_rect(),
  sk_canvas:draw_rect(Canvas, Rect, Paint),
  Picture = sk_picture:from_canvas(Canvas),
  frame_buffer_server:submit_frame(Ref, {{X * 1.0, Y * 1.0}, Picture}).

pick_color(grabbed) -> sk_color:red();
pick_color(free) -> sk_color:blue().

make_rect() -> sk_rect:make_xywh(0, 0, 100, 100).
