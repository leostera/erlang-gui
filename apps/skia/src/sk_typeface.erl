-module(sk_typeface).

-export([ default/0
        , new/2
        ]).

default() -> skia_native:sk_typeface__default().

new(Name, FontStyle) when is_binary(Name) ->
  skia_native:sk_typeface__new(Name, FontStyle).
