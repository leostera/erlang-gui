-module(fps).

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

init(_) ->
  chalk_pipeline:register(fun ?MODULE:draw/0),
  {ok, #{ time => erlang:system_time()
        , counter => 0
        , frame => frame(0)
        }}.

terminate(_, _) -> ok.

handle_call(draw, _From, State) -> do_draw(State);
handle_call(_Msg, _From, State) -> {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

%%==============================================================================
%% Api
%%==============================================================================

start() -> start_link([],[]).

restart() -> gen_server:stop(?MODULE), start().

draw() -> gen_server:call(?MODULE, draw).

%%==============================================================================
%% Internal
%%==============================================================================

do_draw(State=#{ time := T0, counter := C0, frame := F0 }) ->
  Now = erlang:system_time(),
  {F1, C1, T1} = case (Now - T0)/1000000 > 1000 of
                             true -> {frame(C0), 0, Now};
                             false -> {F0, C0+1, T0}
                           end,
  { reply
  , {ok, {0.0,90.0,0.0}, F1}
  , State#{ frame => F1
          , counter => C1
          , time => T1 }}.

frame(Count) ->
  Text = "DPS: " ++ integer_to_list(round(Count)),
  Canvas = sk_canvas:new(240, 80),

  Paint = sk_paint:new(),
  sk_paint:set_style(Paint, sk_paint:style_stroke_and_fill()),
  sk_paint:set_stroke_width(Paint, 3.0),
  sk_paint:set_color(Paint, sk_color:rgba( round(255*0.3)
                                         , round(255*0.3)
                                         , round(255*0.3)
                                         , round(255*1.0))),
  Rect = sk_rect:make_xywh(0, 0, 240, 80),
  sk_canvas:draw_rect(Canvas, Rect, Paint),

  TextPaint = sk_paint:new(),
  sk_paint:set_style(TextPaint, sk_paint:style_stroke_and_fill()),
  sk_paint:set_stroke_width(TextPaint, 1.0),
  sk_paint:set_color(TextPaint, color(Count)),
  Font = sk_font:default(),
  sk_font:set_size(Font, 40.0),
  TextBlob = sk_text_blob:from_binary(Text, Font),
  sk_canvas:draw_text_blob(Canvas, TextBlob, 25, 52, TextPaint),

  sk_picture:from_canvas(Canvas).

color(X) when X < 20 -> sk_color:rgba(255, 40, 40, 255);
color(X) when X < 40 -> sk_color:rgba(255, 255, 40, 255);
color(X) when X < 60 -> sk_color:rgba(80, 80, 255, 255);
color(X) when X >= 60 -> sk_color:rgba(40, 255, 40, 255).
