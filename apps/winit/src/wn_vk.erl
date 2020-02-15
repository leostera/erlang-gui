-module(wn_vk).

-export([
         required_extensions/0
        ]).

-compile(no_native).
-on_load(on_load/0).

-define(WINIT_VULKAN_SO_NAME, libwinit).
-define(NOT_IMPLEMENTED,
        erlang:nif_error(nif_not_loaded,module,?MODULE,line,?LINE)).

on_load() -> erlang:load_nif(?WINIT_VULKAN_SO_NAME, ops).

required_extensions() -> ?NOT_IMPLEMENTED.
