-module(vk_instance).

-compile(no_native).
-on_load(on_load/0).

-record(vk_instance, { }).

-opaque t() :: #vk_instance{}.

create(CreateInfo) ->
  {ok, #vk_instance{}}.

