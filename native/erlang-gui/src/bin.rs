extern crate crossbeam;
extern crate crossbeam_utils;

use std::io;
use std::sync::{Arc, Mutex};

use crossbeam::queue::SegQueue;
use crossbeam_utils::thread;

use erletf::Eterm;

#[macro_use]
mod event_codec;

mod beam_io;
mod render_loop;

pub fn main() {
    let current_frame: Arc<Mutex<Option<Vec<u8>>>> = Arc::new(Mutex::new(None));
    let commands_queue = SegQueue::<Eterm>::new();

    let mut stdin = io::stdin();
    let mut stdout = io::stdout();

    thread::scope(|s| {
        s.spawn(|_| {
            beam_io::command_processor(&mut stdout, &commands_queue, current_frame.clone())
        });
        s.spawn(|_| beam_io::reader(&mut stdin, &commands_queue));
        render_loop::run(current_frame.clone(), &commands_queue);
    });
}
