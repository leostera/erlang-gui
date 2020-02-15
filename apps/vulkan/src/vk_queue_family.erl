-module(vk_queue_family).

-compile(no_native).
-on_load(on_load/0).

-record(vk_queue_family, { }).

-opaque t() :: #vk_queue_family{}.

enumerate(PhysicalDevice) -> [].
