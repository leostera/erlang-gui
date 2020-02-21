-module(sk_picture).

-export([ from_canvas/1
        , as_bytes/1
        ]).

from_canvas(Canvas) -> skia_native:sk_picture__from_canvas(Canvas).

as_bytes(Picture) -> skia_native:sk_picture__as_bytes(Picture).
