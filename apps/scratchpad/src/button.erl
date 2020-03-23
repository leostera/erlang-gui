-module(button).

-compile([export_all]).

canvas() ->
  Canvas = sk_canvas:new(400, 200),
  sk_canvas:clip_rect(Canvas, 400, 200),

  Paint = sk_paint:new(),
  sk_paint:set_stroke_width(Paint, 20.0),
  sk_paint:set_color(Paint, sk_color:rgba(222, 202,30, 127)),
  Rect = sk_rect:make_xywh(0, 0, 400, 200),
  sk_canvas:draw_rect(Canvas, Rect, Paint),

  Paint2 = sk_paint:new(),
  sk_paint:set_color(Paint2, sk_color:rgba(22, 22, 22, 255)),
  Font = sk_font:new([{size, 80.0}]),
  Text = sk_text_blob:from_binary("Hello, Joe!", Font),
  sk_canvas:draw_text_blob(Canvas, Text, 0, 120, Paint2),

  sk_picture:from_canvas(Canvas).
