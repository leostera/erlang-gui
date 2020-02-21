-module(sk_text_blob).

-export([ from_binary/2
        ]).

from_binary(Text, Font) -> skia_native:sk_text_blob__from_binary(Text, Font).
