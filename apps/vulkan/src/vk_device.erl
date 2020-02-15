-module(vk_device).

-export([
         create/1
        ]).

-record(vk_device, {}).

-opaque t() :: #vk_device{}.

create(PhysicalDevice, CreateInfo) -> {ok, #vk_device{}}.
