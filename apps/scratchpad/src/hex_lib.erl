-module(hex_lib).

-compile([export_all]).

bg_white() -> sk_color:rgba(240,240,240,255).
line_black() -> sk_color:rgba(22,22,22,255).

line_paint() ->
  Paint = sk_paint:new(),
  sk_paint:set_style(Paint, sk_paint:style_stroke()),
  sk_paint:set_stroke_width(Paint, 10.0),
  sk_paint:set_color(Paint, line_black()),
  Paint.

bg() ->
  Canvas = sk_canvas:new(1000, 1000),
  sk_canvas:draw_color(Canvas, bg_white()),
  sk_picture:from_canvas(Canvas).

flat_hex_grid(W, H, Size) ->
  {Rows, Cols} = fit_tiles(Size, W, H),
  Tiles = logical_tiles(Rows, Cols),

  Tile = flat_hex_tile(Size),

  Canvas = sk_canvas:new(W, H),
  sk_canvas:translate(Canvas, Size*1.0, Size*1.0),
  lists:foreach(fun ({X, Y}) ->
                    EvenY = case math:fmod(Y, 2) of 0.0 -> true; _ -> false end,
                    W0 = Size*2.0,
                    H0 = Size*math:sqrt(3),
                    RealX = case EvenY of
                              true -> X *W0*1.50;
                              _ -> X*W0*1.50 + W0*0.75
                            end,
                    RealY = case EvenY of
                              true -> Y*H0-(H0*Y/2.0);
                              _ -> Y*H0*0.5
                            end,
                    sk_canvas:translate(Canvas, RealX, RealY),
                    sk_canvas:draw_picture(Canvas, Tile),
                    sk_canvas:translate(Canvas, -RealX, -RealY)
                end, Tiles),
  sk_picture:from_canvas(Canvas).


fit_tiles(Size, W, H) ->
  Rows = round(W / Size),
  Cols = round(H / Size),
  {Rows, Cols}.

build_tiles(Size, Rows, Cols) ->
  LogTiles = logical_tiles(Rows, Cols),
  place_tiles(Size, LogTiles).

logical_tiles(Rows, Cols) ->
  [ {I, J} || I <- lists:seq(0, Rows) , J <- lists:seq(0, Cols) ].

place_tiles(Size, Tiles) ->
  lists:map(fun ({X, Y}) ->
                    EvenY = case math:fmod(Y, 2) of 0.0 -> true; _ -> false end,
                    W0 = Size*2.0,
                    H0 = Size*math:sqrt(3),
                    RealX = case EvenY of
                              true -> X *W0*1.50;
                              _ -> X*W0*1.50 + W0*0.75
                            end,
                    RealY = case EvenY of
                              true -> Y*H0-(H0*Y/2.0);
                              _ -> Y*H0*0.5
                            end,
                    #{ logical => {X,Y}
                     , physical => {RealX, RealY}
                     }
                end, Tiles).

flat_hex_tile(Size) ->
  Points = flat_hex_points(Size),

  Canvas = sk_canvas:new(Size*2, Size*2),

  Path = polygon(Points),

  sk_canvas:draw_path(Canvas, Path, line_paint()),
  sk_picture:from_canvas(Canvas).


flat_hex_points(Size) ->
  [ hex_point(Size, 0.0)
  , hex_point(Size, 60.0)
  , hex_point(Size, 120.0)
  , hex_point(Size, 180.0)
  , hex_point(Size, 240.0)
  , hex_point(Size, 300.0)
  ].

pointy_hex_points(Size) ->
  [ hex_point(Size, 30.0)
  , hex_point(Size, 90.0)
  , hex_point(Size, 150.0)
  , hex_point(Size, 210.0)
  , hex_point(Size, 270.0)
  , hex_point(Size, 330.0)
  ].

hex_point(Size, AngleDeg) ->
  AngleRad = math:pi() / 180.0 * AngleDeg,
  { Size * math:cos(AngleRad)
  , Size * math:sin(AngleRad)
  }.

polygon([{X,Y}|Rest]) ->
  Path = sk_path:new(),
  sk_path:move_to(Path, X, Y),
  lists:foreach(fun ({X2,Y2}) -> sk_path:line_to(Path, X2, Y2) end, Rest),
  sk_path:line_to(Path, X, Y),
  Path.
