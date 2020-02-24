The idea here is to build a high-performance massively parallel graphical user
system by binding to Skia and Vulkan to Erlang


=== STUFFS TO DO ===

DONE. send/receive erlang terms to the port (not just plain text)

DONE. create a default surface using vulkan
    
     currently learning about vulkan's architecture
     and mapping out the high-level erlang api to interface with it.

DONE. fill in the erlang api gaps with NIFs so the thing actually runs.

      if a window pops up with a color on it, i'm dandy!

DONE. expose canvas/surface as an opaque object to draw on
DONE. create a skia surface as an opaque object too
DONE. draw on the window surface using Skia NIFs
DONE. render skia surface on the vulkan surface 

5) setup an event handler for winit to get events into erlang



=== NOTES === 

There will be a Port Driver that works as as our RenderLoop and as our
EventLoop.

The Port Driver will handle incoming primitive drawing commands from Erlang, and
it will relay events from the window.

From Erlang, a set of NIFs will create Skia resources that can be shared around
the BEAM, and ultimately will be sent to the Port Driver to display on screen.



       \
       |---[ port ]--> EventLoop + RenderLoop?
       |                   ^
       |                   |
Erlang |      [ opaque resources in memoyy ]
       |                   ^
       |                   |
       |---[ nif  ]-->  
       /



To render, we need to have:
1) Instance, Device
2) Window, Surface
3) Queue, SwapChain, Images
4) Pipeline, FrameBuffer
in the same thread.

To create drawing commands we need:
1) Instance, Device, QueueFamily
