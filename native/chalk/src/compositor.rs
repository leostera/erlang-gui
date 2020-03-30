extern crate crossbeam;

use std::sync::Arc;
use std::sync::Mutex;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use crossbeam::queue::ArrayQueue;

use crate::command::{CommandIn, CommandOut};
use crate::fps_counter::FpsCounter;

use skia_safe::{Picture, Rect};
use skulpin::{CoordinateSystem, Renderer, RendererBuilder};
use winit::dpi::LogicalSize;
use winit::event::Event;
use winit::event_loop::EventLoop;
use winit::platform::desktop::EventLoopExtDesktop;
use winit::window::{Window, WindowBuilder};

pub struct Compositor<'a> {
    event_loop: Option<EventLoop<()>>,

    renderer: Renderer,

    window: Window,

    current_frame: Picture,
    last_frame_request_time: u128,

    commands_in: &'a ArrayQueue<CommandIn>,
    commands_out: std::sync::mpsc::Sender<CommandOut>,

    fps_counter: FpsCounter,
}

impl<'a> Compositor<'a> {
    pub fn new(
        commands_in: &'a ArrayQueue<CommandIn>,
        // commands_out: &'a ArrayQueue<CommandOut>,
        commands_out: std::sync::mpsc::Sender<CommandOut>,
    ) -> Compositor<'a> {
        let event_loop = EventLoop::<()>::with_user_event();

        let w = 3840.0;
        let h = 2160.0;
        let logical_size = LogicalSize::new(w, h);

        let visible_range = Rect {
            left: 0.0,
            right: logical_size.width as f32,
            top: 0.0,
            bottom: logical_size.height as f32,
        };
        let scale_to_fit = skulpin::skia_safe::matrix::ScaleToFit::Center;

        let window = WindowBuilder::new()
            .with_title("Chalk")
            .with_inner_size(logical_size)
            .build(&event_loop)
            .expect("Failed to create window");

        window.set_cursor_visible(true);

        let renderer = RendererBuilder::new()
            .use_vulkan_debug_layer(true)
            .coordinate_system(CoordinateSystem::VisibleRange(visible_range, scale_to_fit))
            .prefer_discrete_gpu()
            .prefer_mailbox_present_mode()
            .build(&window)
            .unwrap();

        let current_frame = Picture::new_placeholder(Rect::new(0.0, 0.0, w, h));

        let fps_counter = FpsCounter::new();

        Compositor {
            commands_in,
            commands_out,
            current_frame,
            event_loop: Some(event_loop),
            last_frame_request_time: 0,
            fps_counter,
            renderer,
            window,
        }
    }

    pub fn run(mut self, should_exit: Arc<Mutex<bool>>) -> () {
        self.commands_out.send(CommandOut::RequestFrame);
        self.event_loop
            .take()
            .unwrap()
            .run_return(move |event, _target, _control_flow| {
                if should_exit.is_poisoned() {
                    std::process::exit(0);
                }
                self.step(event);
            })
    }

    pub fn handle_commands(&mut self, current_frame_time: u128) -> () {
        match self.commands_in.pop() {
            Ok(CommandIn::Echo(term)) => {
                self.commands_out.send(CommandOut::Echo(term));
            }
            Ok(CommandIn::Render(new_frame)) => {
                self.current_frame = new_frame;
                self.commands_out.send(CommandOut::RequestFrame);
                self.window.request_redraw();
                self.last_frame_request_time = current_frame_time;
            }
            _ => (),
        };
    }

    pub fn step(&mut self, event: Event<'_, ()>) -> () {
        let current_frame_time = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("Time went backwards")
            .as_millis();

        self.handle_commands(current_frame_time);

        match event {
            Event::MainEventsCleared => {
                self.window.request_redraw();
            }

            Event::RedrawRequested(_windows_id) => {
                if (current_frame_time - self.last_frame_request_time > 6) {
                    self.commands_out.send(CommandOut::RequestFrame);
                }

                let current_frame = &mut self.current_frame;
                let commands_out = &mut self.commands_out;
                let fps = &mut self.fps_counter;
                self.renderer
                    .draw(&self.window, |canvas, _coordinate_system_helper| {
                        canvas.draw_picture(current_frame, None, None);
                        fps.tick();
                        fps.draw(canvas);
                    });
            }

            event => {
                if let Some(e) = crate::event_codec::to_command(event) {
                    self.commands_out.send(e);
                }
            }
        };
    }
}
