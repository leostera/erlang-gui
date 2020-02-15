-module(wn_window_builder).

-export([ build/1
        , build_vulkan_surface/2
        , new/0
        , with_always_on_top/2
        , with_decorations/2
        , with_fullscreen/2
        , with_inner_size/2
        , with_max_inner_size/2
        , with_maximized/2
        , with_min_inner_size/2
        , with_resizable/2
        , with_title/2
        , with_transparent/2
        , with_visible/2
        , with_window_icon/2
        ]).

-export_type([
              t/0
             ]).

-record(window_builder, {

         }).

-opaque t() :: #window_builder{}.

new() -> #{}.

build(#window_builder{}=_T) -> ok.

with_always_on_top(#window_builder{}=_T, _I) -> ok.
with_decorations(#window_builder{}=_T, _I) -> ok.
with_fullscreen(#window_builder{}=_T, _I) -> ok.
with_inner_size(#window_builder{}=_T, _I) -> ok.
with_max_inner_size(#window_builder{}=_T, _I) -> ok.
with_maximized(#window_builder{}=_T, _I) -> ok.
with_min_inner_size(#window_builder{}=_T, _I) -> ok.
with_resizable(#window_builder{}=_T, _I) -> ok.
with_title(#window_builder{}=_T, _I) -> ok.
with_transparent(#window_builder{}=_T, _I) -> ok.
with_visible(#window_builder{}=_T, _I) -> ok.
with_window_icon(#window_builder{}=_T, _I) -> ok.

build_vulkan_surface(_I, #window_builder{}=_T) -> ok.
