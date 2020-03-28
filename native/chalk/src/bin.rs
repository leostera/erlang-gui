extern crate crossbeam;
extern crate crossbeam_utils;

use std::sync::Arc;
use std::sync::Mutex;

use std::sync::mpsc::channel;

#[macro_use]
mod beam_io;

mod command;
mod compositor;
mod event_codec;
mod fps_counter;

pub fn main() {
    let in_queue = crossbeam::queue::ArrayQueue::<command::CommandIn>::new(100);

    let (out_send, out_recv) = channel();

    let mut stdin = std::io::stdin();
    let mut stdout = std::io::stdout();

    let should_exit = Arc::new(Mutex::new(false));
    let compositor = compositor::Compositor::new(&in_queue, out_send);

    let _ = crossbeam::thread::scope(|s| {
        s.spawn(|_| {
            let should_exit = should_exit.clone();
            let _lock = should_exit.lock().unwrap();
            beam_io::BeamReader::new(&mut stdin, &in_queue).read_loop();
        });
        s.spawn(|_| {
            beam_io::BeamWriter::new(&mut stdout, out_recv).flush_loop();
        });
        compositor.run(should_exit.clone());
    });

    ()
}
