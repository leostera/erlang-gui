-module(sk_rect).

-export([ make_xywh/4
        ]).

make_xywh(X,Y,W,H) -> skia_native:sk_rect__make_xywh(X,Y,W,H).
