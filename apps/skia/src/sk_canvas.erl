-module(sk_canvas).

-export([ new/2
        , clear/2
        , clip_rect/3
        , draw_circle/5
        , draw_color/2
        , draw_image/3
        , draw_image_rect/4
        , draw_paint/2
        , draw_path/3
        , draw_picture/2
        , draw_rect/3
        , draw_round_rect/5
        , draw_rrect/3
        , draw_text_blob/5
        , translate/3
        ]).

new(W, H) -> skia_native:sk_canvas__new(W, H).

clear(C, Color) -> skia_native:sk_canvas__clear(C, Color).

translate(C, X, Y) when is_float(X) and is_float(Y) -> skia_native:sk_canvas__translate(C, X, Y).

draw_color(C, Color) -> skia_native:sk_canvas__draw_color(C, Color).

draw_paint(C, Paint) -> skia_native:sk_canvas__draw_paint(C, Paint).

draw_rect(C, Rect, Paint) -> skia_native:sk_canvas__draw_rect(C, Rect, Paint).

draw_rrect(C, Rect, Paint) -> skia_native:sk_canvas__draw_rrect(C, Rect, Paint).

draw_circle(C, Radius, X, Y, Paint) -> skia_native:sk_canvas__draw_circle(C, Radius, X, Y, Paint).

draw_round_rect(C, Rect, X, Y, Paint) -> skia_native:sk_canvas__draw_round_rect(C, Rect, X, Y, Paint).

draw_path(C, Path, Paint) -> skia_native:sk_canvas__draw_path(C, Path, Paint).

draw_image(C, Rect, Paint) -> skia_native:sk_canvas__draw_image(C, Rect, Paint).

draw_image_rect(C, Image, Rect, Paint) -> skia_native:sk_canvas__draw_image_rect(C, Image, Rect, Paint).

draw_text_blob(C, Text, X, Y, Paint) -> skia_native:sk_canvas__draw_text_blob(C, Text, X, Y, Paint).

draw_picture(C, Picture) -> skia_native:sk_canvas__draw_picture(C, Picture).

clip_rect(C, W, H) when is_integer(W) and is_integer(H) -> skia_native:sk_canvas__clip_rect(C, W, H).
