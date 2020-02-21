-module(sk_path).

-export([ new/0
        , close/1
        , line_to/3
        , cubic_to/4
        ]).

new() -> skia_native:sk_path__new().

close(P) -> skia_native:sk_path__close(P).

line_to(P, X, Y) -> skia_native:sk_path__line_to(P, X, Y).

cubic_to(P, {X, Y}, {X2, Y2}, {X3, Y3}) -> skia_native:sk_path__cubic(P, X, Y, X2, Y2, X3, Y3).
