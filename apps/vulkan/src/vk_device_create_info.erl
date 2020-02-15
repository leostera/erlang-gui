-module(vk_device_create_info).

-export([ new/0
        , with_queue_create_infos/2
        , with_device_features/2
        ]).

-export_type([ t/0 ]).

-record(vk_device_create_info, { }).

-opaque t() :: #vk_device_create_info{}.

new() -> #vk_device_create_info{}.

with_queue_create_infos(Queues, #vk_device_create_info{}=Info) -> ok.

with_features(Feats, #vk_device_create_info{}=Info) -> ok.
