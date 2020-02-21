-module(sk_rrect).

-export([ new/0
        , set_oval/2
        , offset/3
        ]).

new() -> skia_native:sk_rrect__new().

set_oval(O, Rect) -> skia_native:sk_rrect__set_oval(O, Rect).

offset(R, X, Y) -> skia_native:sk_rrect__offset(R, X, Y).

