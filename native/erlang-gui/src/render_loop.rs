extern crate crossbeam;

use std::time::{SystemTime, UNIX_EPOCH};

use crossbeam::queue::SegQueue;

use erletf::Eterm;

use skia_safe::Picture;

use winit::event::{ElementState, Event, MouseButton, WindowEvent};
use winit::event_loop::EventLoop;
use winit::platform::desktop::EventLoopExtDesktop;

pub fn run(render_queue: &SegQueue<Vec<u8>>, command_queue: &SegQueue<Eterm>) -> () {
    let mut event_loop = EventLoop::<()>::with_user_event();

    // Set up the coordinate system to be fixed at 31, and use this as the default window size
    // This means the drawing code can be written as though the window is always 900x600. The
    // output will be automatically scaled so that it's always visible.
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
        .with_title("Erlang Renderer")
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
                //
                // Halt if the user requests to close the window
                //
                winit::event::Event::WindowEvent {
                    event: winit::event::WindowEvent::CloseRequested,
                    ..
                } => *control_flow = winit::event_loop::ControlFlow::Exit,

                //
                // Close if the escape key is hit
                //
                winit::event::Event::WindowEvent {
                    event:
                        winit::event::WindowEvent::KeyboardInput {
                            input:
                                winit::event::KeyboardInput {
                                    virtual_keycode: Some(winit::event::VirtualKeyCode::Escape),
                                    ..
                                },
                            ..
                        },
                    ..
                } => *control_flow = winit::event_loop::ControlFlow::Exit,
                //
                // Request a redraw any time we finish processing events
                //
                Event::MainEventsCleared => {
                    // Queue a RedrawRequested event.
                    window.request_redraw();
                }
                Event::WindowEvent { event, .. } => {
                    if let Some(e) = relay_window_event(event) {
                        command_queue.push(e)
                    };
                }
                Event::RedrawRequested(_windows_id) => {
                    if let Ok(frame) = render_queue.pop() {
                        command_queue.push(log_render_begin());
                        if let Err(e) =
                            renderer.draw(&window, |canvas, _coordinate_system_helper| {
                                draw(frame, canvas);
                                frame_count += 1;
                            })
                        {
                            println!("Error during draw: {:?}", e);
                            *control_flow = winit::event_loop::ControlFlow::Exit
                        };
                        command_queue.push(log_render_end());
                    }
                }

                _event => (), //println!("{:?}", event),
            }
        });
    }
}

fn draw(frame: Vec<u8>, canvas: &mut skia_safe::Canvas) -> () {
    let picture = Picture::from_bytes(&frame).unwrap();
    picture.playback(canvas);
}

/*==============================================================================
 *
 * Auxiliary events for logging
 *
 *============================================================================*/

fn log_render_begin() -> Eterm {
    Eterm::Tuple(vec![
        Eterm::Atom(String::from("no_ref")),
        Eterm::Atom(String::from("relay")),
        Eterm::Tuple(vec![
            Eterm::Atom(String::from("render_begin")),
            Eterm::Atom(
                SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .expect("Time went backwards")
                    .as_millis()
                    .to_string(),
            ),
        ]),
    ])
}

fn log_render_end() -> Eterm {
    Eterm::Tuple(vec![
        Eterm::Atom(String::from("no_ref")),
        Eterm::Atom(String::from("relay")),
        Eterm::Tuple(vec![
            Eterm::Atom(String::from("render_end")),
            Eterm::Atom(
                SystemTime::now()
                    .duration_since(UNIX_EPOCH)
                    .expect("Time went backwards")
                    .as_millis()
                    .to_string(),
            ),
        ]),
    ])
}

/*==============================================================================
 *
 * Relaying of Events
 *
 *============================================================================*/

fn relay_window_event<'a>(event: WindowEvent<'a>) -> Option<Eterm> {
    let ev = match event {
        WindowEvent::MouseInput { state, button, .. } => Some(Eterm::Tuple(vec![
            Eterm::Atom(String::from("event")),
            Eterm::Atom(String::from("mouse_input")),
            match state {
                ElementState::Pressed => Eterm::Atom(String::from("pressed")),
                ElementState::Released => Eterm::Atom(String::from("released")),
            },
            match button {
                MouseButton::Left => Eterm::Atom(String::from("left")),
                MouseButton::Right => Eterm::Atom(String::from("right")),
                MouseButton::Middle => Eterm::Atom(String::from("middle")),
                _ => Eterm::Atom(String::from("other")),
            },
        ])),
        _ => None,
    };
    ev.map(|e| {
        Eterm::Tuple(vec![
            Eterm::Atom(String::from("no_ref")),
            Eterm::Atom(String::from("relay")),
            e,
        ])
    })
}
