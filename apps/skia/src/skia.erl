-module(skia).

-export([ canvas/2
        ]).

canvas(W, H) -> skia_nif:new(W, H).
