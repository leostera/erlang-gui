-module(skia_native).

-export([
         sk_canvas__clear/2,
         sk_canvas__clip_rect/3,
         sk_canvas__draw_circle/5,
         sk_canvas__draw_color/2,
         sk_canvas__draw_paint/2,
         sk_canvas__draw_path/3,
         sk_canvas__draw_picture/2,
         sk_canvas__draw_pictures/2,
         sk_canvas__draw_rect/3,
         sk_canvas__draw_round_rect/5,
         sk_canvas__draw_rrect/3,
         sk_canvas__draw_text_blob/5,
         sk_canvas__new/2,
         sk_canvas__translate/3,
         sk_color__blue/0,
         sk_color__cyan/0,
         sk_color__green/0,
         sk_color__red/0,
         sk_color__rgba/4,
         sk_color__yellow/0,
         sk_font__default/0,
         sk_font__new/1,
         sk_font__set_size/2,
         sk_font_style__default/0,
         sk_font_style__new/3,
         sk_font_style__slant/1,
         sk_font_style__weight/1,
         sk_font_style__width/1,
         sk_paint__new/0,
         sk_paint__set_color/2,
         sk_paint__set_stroke_width/2,
         sk_paint__set_style/2,
         sk_paint__style_fill/0,
         sk_paint__style_stroke/0,
         sk_paint__style_stroke_and_fill/0,
         sk_path__close/1,
         sk_path__cubic/7,
         sk_path__line_to/3,
         sk_path__move_to/3,
         sk_path__new/0,
         sk_picture__as_bytes/1,
         sk_picture__from_canvas/1,
         sk_rect__make_xywh/4,
         sk_rrect__new/0,
         sk_rrect__offset/3,
         sk_rrect__set_oval/2,
         sk_text_blob__from_binary/2,
         sk_typeface__default/0,
         sk_typeface__new/2
        ]).

-compile(no_native).
-on_load(on_load/0).

on_load() ->
  SoName = code:priv_dir(skia) ++ "/native/liberlskia",
  erlang:load_nif(SoName, ok).

-define(NIF_ERROR, erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE})).

sk_canvas__clear(_C, _Color) -> ?NIF_ERROR.
sk_canvas__clip_rect(_C, _X, _Y) -> ?NIF_ERROR.
sk_canvas__draw_circle(_C, _Radius, _X, _Y, _Paint) -> ?NIF_ERROR.
sk_canvas__draw_color(_C, _Color) -> ?NIF_ERROR.
sk_canvas__draw_paint(_C, _Paint) -> ?NIF_ERROR.
sk_canvas__draw_path(_C, _Path, _Paint) -> ?NIF_ERROR.
sk_canvas__draw_picture(_C, _Picture) -> ?NIF_ERROR.
sk_canvas__draw_pictures(_C, _Pictures) -> ?NIF_ERROR.
sk_canvas__draw_rect(_C, _Rect, _Paint) -> ?NIF_ERROR.
sk_canvas__draw_round_rect(_C, _Rect, _X, _Y, _Paint) -> ?NIF_ERROR.
sk_canvas__draw_rrect(_C, _Rect, _Paint) -> ?NIF_ERROR.
sk_canvas__draw_text_blob(_C, _Text, _X, _Y, _Paint) -> ?NIF_ERROR.
sk_canvas__new(_W, _H) -> ?NIF_ERROR.
sk_canvas__translate(_C, _X, _Y) -> ?NIF_ERROR.
sk_color__blue() -> ?NIF_ERROR.
sk_color__cyan() -> ?NIF_ERROR.
sk_color__green() -> ?NIF_ERROR.
sk_color__red() -> ?NIF_ERROR.
sk_color__rgba(_R, _G, _B, _A) -> ?NIF_ERROR.
sk_color__yellow() -> ?NIF_ERROR.
sk_font__default() -> ?NIF_ERROR.
sk_font__new(_Typeface) -> ?NIF_ERROR.
sk_font__set_size(_F, _S) -> ?NIF_ERROR.
sk_paint__new() -> ?NIF_ERROR.
sk_paint__set_color(_Paint, _Color) -> ?NIF_ERROR.
sk_paint__set_stroke_width(_Paint, _Width) -> ?NIF_ERROR.
sk_paint__set_style(_Paint, _Style) -> ?NIF_ERROR.
sk_paint__style_fill() -> ?NIF_ERROR.
sk_paint__style_stroke() -> ?NIF_ERROR.
sk_paint__style_stroke_and_fill() -> ?NIF_ERROR.
sk_path__close(_P) -> ?NIF_ERROR.
sk_path__cubic(_P, _X, _Y, _X2, _Y2, _X3, _Y) -> ?NIF_ERROR.
sk_path__line_to(_P, _X, _Y) -> ?NIF_ERROR.
sk_path__move_to(_P, _X, _Y) -> ?NIF_ERROR.
sk_path__new() -> ?NIF_ERROR.
sk_picture__as_bytes(_Picture) -> ?NIF_ERROR.
sk_picture__from_canvas(_Canvas) -> ?NIF_ERROR.
sk_rect__make_xywh(_X,_Y,_W,_H) -> ?NIF_ERROR.
sk_rrect__new() -> ?NIF_ERROR.
sk_rrect__offset(_R, _X, _Y) -> ?NIF_ERROR.
sk_rrect__set_oval(_Oval, _Rect) -> ?NIF_ERROR.
sk_text_blob__from_binary(_Text, _Font) -> ?NIF_ERROR.
sk_typeface__default() -> ?NIF_ERROR.
sk_typeface__new(_Name, _FontStyle) -> ?NIF_ERROR.
sk_font_style__default() -> ?NIF_ERROR.
sk_font_style__new(Weight, Width, Slant) -> ?NIF_ERROR.
sk_font_style__weight(Self) -> ?NIF_ERROR.
sk_font_style__width(Self) -> ?NIF_ERROR.
sk_font_style__slant(Self) -> ?NIF_ERROR.
