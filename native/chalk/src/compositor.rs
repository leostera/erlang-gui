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

    current_frame: Option<Picture>,

    commands_in: &'a ArrayQueue<CommandIn>,
    // commands_out: &'a ArrayQueue<CommandOut>,
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

        let logical_size = LogicalSize::new(3840.0, 2160.0);

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

        let current_frame = None;

        let fps_counter = FpsCounter::new();

        Compositor {
            commands_in,
            commands_out,
            current_frame,
            event_loop: Some(event_loop),
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

    pub fn handle_commands(&mut self) -> () {
        match self.commands_in.pop() {
            Ok(cmd) => {
                let reply = match cmd {
                    CommandIn::Echo(term) => CommandOut::Echo(term),
                    CommandIn::Render(new_frame) => {
                        self.current_frame = Some(new_frame);
                        self.window.request_redraw();
                        CommandOut::RenderQueued
                    }
                };
                self.commands_out.send(reply);
            }
            _ => (),
        };
    }

    pub fn step(&mut self, event: Event<'_, ()>) -> () {
        self.handle_commands();
        match event {
            Event::MainEventsCleared => {
                self.window.request_redraw();
            }

            Event::RedrawRequested(_windows_id) => {
                let current_frame = &mut self.current_frame;
                let commands_out = &mut self.commands_out;
                let fps = &mut self.fps_counter;
                self.renderer
                    .draw(&self.window, |mut canvas, _coordinate_system_helper| {
                        if let Some(frame) = current_frame {
                            frame.playback(&mut canvas);
                        }
                        fps.tick();
                        fps.draw(canvas);
                        commands_out.send(CommandOut::RequestFrame);
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
