-module(vk_device_queue_create_info).

-export([ new/0
        , with_count/2
        , with_family_index/2
        , with_priority/2
        ]).

-export_type([ t/0 ]).

-record(vk_device_queue_create_info, { }).

-opaque t() :: #vk_device_queue_create_info{}.

new() -> #vk_device_queue_create_info{}.

with_family_index(Index, #vk_device_queue_create_info{}=Info) -> ok.
with_count(Count, #vk_device_queue_create_info{}=Info) -> ok.
with_priority(Prio, #vk_device_queue_create_info{}=Info) -> ok.
