-module(skia_samples).

-export([ a/0
				, b/0
				, clear/0
        ]).

clear() ->
	Canvas = skia_canvas:new(2048, 2048),
	skia_canvas:fill(Canvas),
	skia_canvas:as_bytes(Canvas).

a() ->
  io:format("begin ~p\n", [erlang:system_time(millisecond)]),
  Canvas = skia_canvas:new(3840, 2160),
  io:format("new canvas ~p\n", [erlang:system_time(millisecond)]),
  % Do some drawing!
  skia_canvas:scale(Canvas, 1.2, 1.2),
  skia_canvas:move_to(Canvas, 36.0, 48.0),
  skia_canvas:quad_to(Canvas, 660.0, 880.0, 1200.0, 360.0),
  skia_canvas:translate(Canvas, 10.0, 10.0),
  skia_canvas:set_line_width(Canvas, 20.0),
  skia_canvas:stroke(Canvas),
  skia_canvas:save(Canvas),
  skia_canvas:move_to(Canvas, 30.0, 90.0),
  skia_canvas:line_to(Canvas, 110.0, 20.0),
  skia_canvas:line_to(Canvas, 240.0, 130.0),
  skia_canvas:line_to(Canvas, 60.0, 130.0),
  skia_canvas:line_to(Canvas, 190.0, 20.0),
  skia_canvas:line_to(Canvas, 270.0, 90.0),
  skia_canvas:fill(Canvas),
  io:format("drawn ~p\n", [erlang:system_time(millisecond)]),
  % Get bytes out and dump them on an image
  Bytes = skia_canvas:as_bytes(Canvas),
  io:format("bytes ~p\n", [erlang:system_time(millisecond)]),
  Bytes.

b() ->
  Canvas = skia_canvas:new(800, 600),
  % Do some drawing!
  skia_canvas:quad_to(Canvas, 660.0, 880.0, 1200.0, 360.0),
  skia_canvas:set_line_width(Canvas, 200.0),
  skia_canvas:move_to(Canvas, 36.0, 48.0),
  skia_canvas:translate(Canvas, 10.0, 10.0),
  skia_canvas:quad_to(Canvas, 660.0, 880.0, 1200.0, 360.0),
  skia_canvas:set_line_width(Canvas, 20.0),
  skia_canvas:stroke(Canvas),
  skia_canvas:save(Canvas),
  skia_canvas:move_to(Canvas, 30.0, 90.0),
  skia_canvas:line_to(Canvas, 110.0, 20.0),
  skia_canvas:line_to(Canvas, 240.0, 130.0),
  skia_canvas:line_to(Canvas, 60.0, 130.0),
  skia_canvas:line_to(Canvas, 190.0, 20.0),
  skia_canvas:line_to(Canvas, 270.0, 90.0),
  skia_canvas:fill(Canvas),
  % Get bytes out and dump them on an image
  skia_canvas:as_bytes(Canvas).
