extern crate crossbeam;
extern crate crossbeam_utils;

use std::io;

use crossbeam::queue::SegQueue;
use crossbeam_utils::thread;

use erletf::Eterm;

#[macro_use]
mod event_codec;

mod beam_io;
mod render_loop;

pub fn main() {
    let render_queue = SegQueue::<Vec<u8>>::new();
    let commands_queue = SegQueue::<Eterm>::new();

    let mut stdin = io::stdin();
    let mut stdout = io::stdout();

    thread::scope(|s| {
        s.spawn(|_| beam_io::command_processor(&mut stdout, &commands_queue, &render_queue));
        s.spawn(|_| beam_io::reader(&mut stdin, &commands_queue));
        render_loop::run(&render_queue, &commands_queue);
    });
}
