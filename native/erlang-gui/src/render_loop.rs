extern crate crossbeam;

use crate::event_codec::AsErlangTerm;

use std::time::{SystemTime, UNIX_EPOCH};

use crossbeam::queue::SegQueue;

use erletf::Eterm;

use skia_safe::Picture;

use winit::event::{ElementState, Event, MouseButton, WindowEvent};
use winit::event_loop::EventLoop;
use winit::platform::desktop::EventLoopExtDesktop;

pub fn run(render_queue: &SegQueue<Vec<u8>>, command_queue: &SegQueue<Eterm>) -> () {
    let mut event_loop = EventLoop::<()>::with_user_event();

    let logical_size = winit::dpi::LogicalSize::new(3840.0, 2160.0);
    let visible_range = skulpin::skia_safe::Rect {
        left: 0.0,
        right: logical_size.width as f32,
        top: 0.0,
        bottom: logical_size.height as f32,
    };
    let scale_to_fit = skulpin::skia_safe::matrix::ScaleToFit::Center;

    // Create a single window
    let window = winit::window::WindowBuilder::new()
        .with_title("Chalk!")
        .with_inner_size(logical_size)
        .build(&event_loop)
        .expect("Failed to create window");

    // Create the renderer, which will draw to the window
    let renderer = skulpin::RendererBuilder::new()
        .use_vulkan_debug_layer(true)
        .coordinate_system(skulpin::CoordinateSystem::VisibleRange(
            visible_range,
            scale_to_fit,
        ))
        .prefer_discrete_gpu()
        .prefer_fifo_present_mode()
        .build(&window);

    // Check if there were error setting up vulkan
    if let Err(e) = renderer {
        println!("Error during renderer construction: {:?}", e);
        return;
    }

    let mut renderer = renderer.unwrap();

    // Increment a frame count so we can render something that moves
    let mut frame_count = 0;

    loop {
        event_loop.run_return(|event, _window_target, control_flow| {
            match event {
                Event::MainEventsCleared => {
                    // Queue a RedrawRequested event.
                    window.request_redraw();
                }
                Event::RedrawRequested(_windows_id) => {
                    if let Ok(frame) = render_queue.pop() {
                        command_queue.push(log_render_begin());
                        if let Err(e) =
                            renderer.draw(&window, |canvas, _coordinate_system_helper| {
                                let picture = Picture::from_bytes(&frame).unwrap();
                                picture.playback(canvas);
                                frame_count += 1;
                            })
                        {
                            *control_flow = winit::event_loop::ControlFlow::Exit
                        };
                        command_queue.push(log_render_end());
                    }
                }
                event => match relay_event(event) {
                    Some(e) => command_queue.push(e),
                    _ => (),
                },
            }
        });
    }
}

/*==============================================================================
 *
 * Auxiliary events for logging
 *
 *============================================================================*/

fn log_render_begin() -> Eterm {
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards")
        .as_millis()
        .to_string();

    tuple! {
        tuple! { atom! { "ref" }, atom! { "none" } },
        tuple! { atom! { "kind" }, atom! { "relay" } },
        tuple! {
            atom! { "data" },
            tuple! {
                tuple! { atom! {"status"}, atom! { "render_begin" }},
                tuple! { atom! {"time"}, atom! { now }}
            }
        }
    }
}

fn log_render_end() -> Eterm {
    let now = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards")
        .as_millis()
        .to_string();

    tuple! {
        tuple! { atom! { "ref" }, atom! { "none" } },
        tuple! { atom! { "kind" }, atom! { "relay" } },
        tuple! {
            atom! { "data" },
            tuple! {
                tuple! { atom! {"status"}, atom! { "render_end" }},
                tuple! { atom! {"time"}, atom! { now }}
            }
        }
    }
}

fn relay_event(event: Event<'_, ()>) -> Option<Eterm> {
    match event {
        Event::LoopDestroyed => None,
        Event::MainEventsCleared { .. } => None,
        Event::NewEvents(_) => None,
        Event::RedrawEventsCleared => None,
        Event::RedrawRequested { .. } => None,
        Event::UserEvent(_) => None,
        _ => Some(tuple! {
            tuple! { atom! { "ref" }, atom! { "none" } },
            tuple! { atom! { "kind" }, atom! { "relay" } },
            tuple! { atom! { "data" }, event.as_erlang_term() }
        }),
    }
}
