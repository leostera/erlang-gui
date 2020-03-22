-module(sk_color).

-export([ blue/0
        , cyan/0
        , green/0
        , red/0
        , yellow/0
        , rgba/4
        , rgb/3
        ]).

blue() -> skia_native:sk_color__blue().
cyan() -> skia_native:sk_color__cyan().
green() -> skia_native:sk_color__green().
red() -> skia_native:sk_color__red().
yellow() -> skia_native:sk_color__yellow().

rgb(R,G,B) when is_integer(R) and is_integer(G) and is_integer(B) ->
  skia_native:sk_color__rgba(R,G,B,255).

rgba(R,G,B,A) when is_integer(R) and is_integer(G) and is_integer(B) and is_integer(A) ->
  skia_native:sk_color__rgba(R,G,B,A).

