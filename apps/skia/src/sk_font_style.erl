-module(sk_font_style).

-export([
         default/0,
         new/3,
         slant/1,
         weight/1,
         width/1
        ]).


default() -> skia_native:sk_font_style__default().
new(Weight, Width, Slant) -> skia_native:sk_font_style__new(Weight, Width, Slant).

weight(Self) -> skia_native:sk_font_style__weight(Self).
width(Self) -> skia_native:sk_font_style__width(Self).
slant(Self) -> skia_native:sk_font_style__slant(Self).
