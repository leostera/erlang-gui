-module(hello_world).

-compile([export_all]).

setup() ->
  {ok, BgRef} = chalk:render({0.0,0.0}, clear(sk_color:cyan())),
  {ok, BtnRef} = chalk:render({0.0,0.0,1.0}, button()),
  {BgRef, BtnRef}.

clear(Color) ->
  Canvas = sk_canvas:new(1000, 1000),
  sk_canvas:draw_color(Canvas, Color),
  sk_picture:from_canvas(Canvas).

button() ->
  Canvas = sk_canvas:new(400, 200),
  sk_canvas:clip_rect(Canvas, 400, 200),

  Paint = sk_paint:new(),
  sk_paint:set_stroke_width(Paint, 20.0),
  sk_paint:set_color(Paint, sk_color:rgba(222, 202,30, 127)),
  Rect = sk_rect:make_xywh(0, 0, 400, 200),
  sk_canvas:draw_rect(Canvas, Rect, Paint),

  Paint2 = sk_paint:new(),
  sk_paint:set_color(Paint2, sk_color:rgba(22, 22, 22, 255)),
  Font = sk_font:new([{size, 30.0}]),
  Text = sk_text_blob:from_binary("Hello, Dungeon Ball Z!", Font),
  sk_canvas:draw_text_blob(Canvas, Text, 0, 120, Paint2),

  sk_picture:from_canvas(Canvas).


sample() ->
  Canvas = sk_canvas:new(1000, 1000),

  Paint = sk_paint:new(),
  %sk_paint:set_style(sk_paint_style:stroke_style()),
  sk_paint:set_stroke_width(Paint, 40.0),
  sk_paint:set_color(Paint, sk_color:red()),

  Rect = sk_rect:make_xywh(500, 700, 400, 600),
  sk_canvas:draw_rect(Canvas, Rect, Paint),

  Oval = sk_rrect:new(),
  sk_rrect:set_oval(Oval, Rect),
  sk_rrect:offset(Oval, 1400, 1600),
  sk_paint:set_color(Paint, sk_color:blue()),

  sk_canvas:draw_rect(Canvas, Rect, Paint),

  sk_paint:set_color(Paint, sk_color:red()),
  sk_canvas:draw_circle(Canvas, 180.0, 1500.0, 1250.0, Paint),

  sk_rrect:offset(Oval, 80, 100),
  sk_paint:set_color(Paint, sk_color:yellow()),
  sk_canvas:draw_round_rect(Canvas, Rect, 800.0, 900.0, Paint),

  Path = sk_path:new(),
  sk_path:cubic_to(Path, {768, 2000}, {512, 256}, {256, 256}),

  sk_paint:set_color(Paint, sk_color:green()),
  sk_canvas:draw_path(Canvas, Path, Paint),

  Paint2 = sk_paint:new(),
  sk_paint:set_color(Paint2, sk_color:blue()),
  Font = sk_font:new([{size, 300.0}]),
  Text = sk_text_blob:from_binary("Hello, Joe!", Font),
  sk_canvas:draw_text_blob(Canvas, Text, 500, 350, Paint2),

  sk_picture:from_canvas(Canvas).

star() ->
  Canvas = sk_canvas:new(1000, 1000),
  Path = sk_path:new(),
  R = 0.45 * 245.0,
  TAU = 6.2831853,
  [
   sk_path:line_to(
     Path,
     R* math:cos( 3 * X * TAU / 7 ),
     R * math:sin( 3 * X * TAU / 7))
   || X <- lists:seq(0,9)
  ],
  sk_path:close(Path),
  Paint = sk_paint:new(),
  sk_canvas:clear(Canvas, sk_color:red()),
  sk_canvas:translate(Canvas, 0.5 * 256.0, 0.5 * 256.0),
  sk_canvas:draw_path(Canvas, Path, Paint),

  sk_picture:from_canvas(Canvas).
