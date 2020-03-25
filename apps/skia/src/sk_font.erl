-module(sk_font).

-export([ default/0
        , new/1
        , set_size/2
        ]).

default() -> skia_native:sk_font__default().

new(Typeface) -> skia_native:sk_font__new(Typeface).

set_size(F, S) -> skia_native:sk_font__set_size(F, S).
