extern crate crossbeam;
extern crate crossbeam_utils;

use std::io;

use crossbeam::queue::SegQueue;
use crossbeam_utils::thread;

use erletf::Eterm;

mod beam_io;
mod render_loop;

pub fn main() {
    let render_queue = SegQueue::<Vec<u8>>::new();
    let commands_queue = SegQueue::<Eterm>::new();

    let mut stdin = io::stdin();
    let mut stdout = io::stdout();

    thread::scope(|s| {
        eprintln!("Starting up Erlang GUI's Vulkan Driver...");
        s.spawn(|_| {
            eprintln!("Firing up Command Processor thread...");
            beam_io::command_processor(&mut stdout, &commands_queue, &render_queue)
        });
        s.spawn(|_| {
            eprintln!("Firing up BEAM IO thread...");
            beam_io::reader(&mut stdin, &commands_queue)
        });

        eprintln!("Firing up Render Loop...");
        render_loop::run(&render_queue, &commands_queue);
    });
    eprintln!("Closing Erlang GUI's Vulkan Driver!");
}
