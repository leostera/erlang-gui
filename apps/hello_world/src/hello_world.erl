-module(hello_world).

-compile([export_all]).

quickstart() ->
  {ok, Instance} = instance(),
  {ok, EvLoop, Surface} = window(Instance),
  {ok, DevAndQueues} = device_and_queue(Instance, Surface),
  {ok, Buffer} = buffer(DevAndQueues),
  {ok, Swapchain, Images} = swapchain(Instance, Surface, DevAndQueues),
  {ok, Pipeline} = gfx_pipeline(),
  {ok, CmdPool} = command_pool(),


instance() ->
  InstanceCreateInfo = vk_instance_create_info:new(),
  vk_instance:create(InstanceCreateInfo).


window(Instance) ->
  {ok, EvLoop} = wn_event_loop:create(),
  B0 = wn_window_builder:new(),
  B1 = wn_window_builder:with_title("Hello Joe", B0),
  B2 = wn_window_builder:with_dimensions({1024,786}, B1),
  {ok, Surface} = wn_window_builder:build_vulkan_surface(Instance, EvLoop, B2),
  {ok, EvLoop, Surface}


device_and_queue(Instance, Surface) ->
  [PhysicalDev|_] = vk_physical_device:enumerate(Instance),
  {ok, Queues} = vk_queue_family:enumerate(PhysicalDev),
  QueueFamilies  = find_queues(Queues),
  QueuesWithPrio = prio_queues(Queues),
  Ext0 = vk_device_extensions:new(),
  Ext1 = vk_device_extensions:enable_khr_swapchain(Ext0),
  true = vk_device_extensions:supported_by_device(PhysicalDev, Ext1),
  DevCreateInfo0 = vk_device_create_info:new(),
  DevCreateInfo1 = vk_device_create_info:with_queue_create_infos(QueueFamilies, DevCreateInfo0),
  DevCreateInfo2 = vk_device_create_info:with_features(Ext1, DevCreateInfo1),
  case vk_device:new(PhysicalDev, DevCreateInfo2) do
     {ok, {Dev, [Gfx]}} -> {ok, {PhysicalDev, Dev, Gfx, Gfx}};
     {ok, {Dev, [Gfx,Pres|_]}} -> {ok, {PhysicalDev, Dev, Gfx, Pres}};
  end.

prio_queues(Qs) ->
  Queues = maps:values(Acc),
  QueuePrio = 1.0,
  lists:map(fun Q -> {Q, QueuePrio} end, Queues).

find_queues(Qs, Surf) ->
  find_queues(Qs, Surf, #{ present => none, graphics => none }).
find_queues([], _Surface, Acc) -> Acc;
find_queues([Q|Queues], Surface, Acc) ->
  Next = case { vk_queue_family:supports_graphics(Q),
              , vk_surface:is_supported(Q, Surface) } of
           {true, true} -> #{ graphics => Q, present => Q };
           {true, _}    -> Acc#{ graphics => Q };
           {_, true}    -> Acc#{ present  => Q };
           _ -> Acc
         end,
  find_queues(Queues, Surface, Next).


buffer({_PhysicalDev, LogicalDev, _GfxQ, _PresentQ}) ->
  vk_cpu_accessible_buffer:from_data(LogicalDev, vk_buffer_usage:all(), 12).


swapchain(Instance, Surface, {PhysicalDev, LogicalDev, GfxQ, PresentQ}) ->
  {ok, Capabilities} = vk_surface:capabilities(PhysicalDev, Surface),
  [SurfaceFormat|_] = vk_surface_capabilities:supported_formats(Capabilities),
  [PresentMode|_] = vk_surface_capabilities:present_modes(Capabilities),
  {ok, {Swapchain, Images}} = vk_swapchain:new(#{
    device => Device,
    surface => Surface,
    num_images => ImageCount,
    format => SurfaceFormat,
    dimensions => Extent,
    layers => Layers,
    usage => ImageUsage,
    sharing_mode => vk_queue:sharing_mode(GfxQ),
    transform => vk_surface_capabilities:current_transform(Capabilities),
    alpha => vk_swapchain_composite_alpha:opaque(),
    mode => PresentMode,
    fullscreen_exclusive => false,
    clipped => true,
    color_space => vk_swapchain_color_space:srgb_non_linear()
   }),
  {ok, Swapchain, Images}.


gfx_pipeline(Device) ->
  P0 = vk_gfx_pipeline:start(),
  P1 = vk_gfx_pipeline:

  {ok, P?}.


command_pool() ->
  CP0 = vk_command_pool_create_info:new(),
  CP1 = vk_command_pool_create_info:with_flags(0, CP0),
  CP2 = vk_command_pool_create_info:with_queue_family_index(GraphicsIndex),
  {ok, Pool} = vk_command_pool:new(Device, CP2),
  {ok, Pool}.
