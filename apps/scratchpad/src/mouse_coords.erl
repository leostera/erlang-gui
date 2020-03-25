-module(mouse_coords).

-behavior(gen_server).

-export([ start_link/2
        , init/1
        , terminate/2
        , handle_cast/2
        , handle_call/3
        ]).

-export([ draw/0
        , start/0
        , restart/0
        ]).

%%==============================================================================
%% Behavior callbacks
%%==============================================================================

start_link(Args, Opts) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

init(Args) ->
  State = initial_state(Args),
  Self = self(),
  chalk_event_server:register(Self),
  chalk_pipeline:register(fun () -> mouse_coords:draw() end),
  {ok, State}.

terminate(_, _) -> ok.

handle_call(draw, _From, State) -> do_draw(State);
handle_call(_Msg, _From, State) -> {noreply, State}.

handle_cast({{type, cursor_moved}, [{x,X}, {y,Y}]}, State) ->
  {noreply, do_cursor_move(State, {X,Y})};
handle_cast(_Msg, State) ->
  {noreply, State}.

%%==============================================================================
%% Api
%%==============================================================================

start() ->
  mouse_coords:start_link([],[]),
  chalk_pipeline:flush().

restart() ->
  gen_server:stop(mouse_coords),
  chalk_pipeline:clear(),
  start().

draw() -> gen_server:call(?MODULE, draw).

%%==============================================================================
%% Internal
%%==============================================================================

initial_state(_) ->
  #{ pos => {1000.0, 1000.0, 10.0}
   , time => erlang:system_time()
   }.

do_cursor_move(State, {X, Y}) -> State#{ pos => {X * 1.0, Y * 1.0, 1.0} }.

do_draw(#{ pos := {X,Y,Z} = Pos
         , time := _
         }=State) ->
  Now = erlang:system_time(),
  Crosshair = crosshair(Pos),
  {reply, {ok, {X - 50.0, Y - 50.0, Z}, Crosshair}, State#{ time => Now }}.


crosshair({X, Y, _}) ->
  C = sk_canvas:new(100, 100),

  P = sk_path:new(),
  sk_path:move_to(P, 0.0, 0.0),
  sk_path:move_to(P, 50.0, 0.0),
  sk_path:line_to(P, 50.0, 100.0),
  sk_path:move_to(P, 0.0, 50.0),
  sk_path:line_to(P, 100.0, 50.0),
  sk_canvas:draw_path(C, P, red_paint()),

  Font = sk_font:new([{size, 30.0}]),
  Text = lists:flatten(io_lib:format("(~p,~p)", [round(X), round(Y)])),
  TextBlob = sk_text_blob:from_binary(Text, Font),
  sk_canvas:draw_text_blob(C, TextBlob, 50, 75, black_paint()),
  sk_picture:from_canvas(C).

red_paint() ->
  Paint = sk_paint:new(),
  sk_paint:set_style(Paint, sk_paint:style_stroke()),
  sk_paint:set_stroke_width(Paint, 3.0),
  sk_paint:set_color(Paint, sk_color:rgba(233,80,80,255)),
  Paint.

black_paint() ->
  Paint = sk_paint:new(),
  sk_paint:set_color(Paint, sk_color:rgba(22, 22, 22, 255)),
  Paint.
