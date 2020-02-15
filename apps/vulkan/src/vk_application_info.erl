-module(vk_application_info).

-compile([ new/0
         , with_api_version/2
         , with_app_name/2
         , with_app_version/2
         , with_engine_name/2
         , with_engine_version/2
         ]).

-record(vk_application_info,
        { app_name,
          app_version,
          engine_name,
          engine_version,
          api_version
        }).

-opaque t() :: #vk_application_info{}.

new() -> #vk_application_info{}.

with_app_name(Name, #vk_application_info{}=AppInfo) -> ok.
with_app_version(Ver, #vk_application_info{}=AppInfo) -> ok.
with_engine_name(Name, #vk_application_info{}=AppInfo) -> ok.
with_engine_version(Ver, #vk_application_info{}=AppInfo) -> ok.
with_api_version(Ver, #vk_application_info{}=AppInfo) -> ok.
