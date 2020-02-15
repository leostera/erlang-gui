-module(vk_physical_device).

-compile(no_native).
-on_load(on_load/0).

-record(vk_physical_device, { }).

-opaque t() :: #vk_physical_device{}.

-spec enumerate(vk_instance:t()) -> [t()].
enumerate(Instance) -> [].
