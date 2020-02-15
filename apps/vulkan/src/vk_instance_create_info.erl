-module(vk_instance_create_info).

-compile([ new/0
         , with_api_version/2
         , with_app_name/2
         , with_app_version/2
         , with_engine_name/2
         , with_engine_version/2
         ]).

-record(vk_instance_create_info,
        { app_info
        , enabled_extension_count
        , enabled_extension_names
        , enabled_layer_count
        }).

-opaque t() :: #vk_instance_create_info{}.

new() -> #vk_instance_create_info{}.

with_app_info(AppInfo, #vk_instance_create_info{}=Info) -> ok.
with_enabled_extension_count(Ext, #vk_instance_create_info{}=Info) -> ok.
with_enabled_extension_names(ExtNames, #vk_instance_create_info{}=Info) -> ok.
with_enabled_layer_count(LayerCount, #vk_instance_create_info{}=Info) -> ok.
