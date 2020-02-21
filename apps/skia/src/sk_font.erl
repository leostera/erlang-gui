-module(sk_font).

-export([ new/0
        , new/1
        , set_size/2
        ]).

new() -> skia_native:sk_font__new().

set_size(F, S) -> skia_native:sk_font__set_size(F, S).

new(Args) -> new(Args, new()).
new([], F) -> F;
new([{size, Size}|T], F) -> new(T, set_size(F, Size)).
