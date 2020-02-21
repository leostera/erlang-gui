-module(sk_color).

-export([ blue/0
        , cyan/0
        , green/0
        , red/0
        , yellow/0
        ]).

blue() -> skia_native:sk_color__blue().
cyan() -> skia_native:sk_color__cyan().
green() -> skia_native:sk_color__green().
red() -> skia_native:sk_color__red().
yellow() -> skia_native:sk_color__yellow().
