-module(sk_paint).

-export([ new/0
        , set_color/2
        , set_stroke_width/2
        , set_style/2
        ]).

new() -> skia_native:sk_paint__new().

set_color(Paint, Color) -> skia_native:sk_paint__set_color(Paint, Color).
set_stroke_width(Paint, Width) when is_float(Width) -> skia_native:sk_paint__set_stroke_width(Paint, Width).
set_style(Paint, Style) -> skia_native:sk_paint__set_style(Paint, Style).


