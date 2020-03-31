extern crate crossbeam;
extern crate crossbeam_utils;

use std::fs::OpenOptions;
use std::io::{BufReader, BufWriter};
use std::path::PathBuf;
use std::sync::{mpsc::channel, Arc, Mutex};

use memmap::{Mmap, MmapMut};

#[macro_use]
mod beam_io;

mod command;
mod compositor;
mod event_codec;
mod fps_counter;

pub fn main() {
    let in_queue = crossbeam::queue::ArrayQueue::<command::CommandIn>::new(100);

    let (out_send, out_recv) = channel();

    let should_exit = Arc::new(Mutex::new(false));
    let compositor = compositor::Compositor::new(&in_queue, out_send);

    let _ = crossbeam::thread::scope(|s| {
        s.spawn(|_| {
            let should_exit = should_exit.clone();
            let _lock = should_exit.lock().unwrap();

            let file = OpenOptions::new()
                .read(true)
                .write(true)
                .open(PathBuf::from(
                    "/home/ostera/repos/github.com/ostera/erlang-gui/chalk.port_in.mmap",
                ))
                .unwrap();

            file.set_len(1024 * 1024 * 1024).unwrap();

            let mmap = unsafe { Mmap::map(&file).unwrap() };

            beam_io::BeamReader::new(mmap, &in_queue).read_loop();
        });

        s.spawn(|_| {
            let file = OpenOptions::new()
                .read(true)
                .write(true)
                .open(PathBuf::from(
                    "/home/ostera/repos/github.com/ostera/erlang-gui/chalk.port_out.mmap",
                ))
                .unwrap();

            file.set_len(1024 * 1024 * 1024).unwrap();

            let mmap = unsafe { MmapMut::map_mut(&file).unwrap() };

            beam_io::BeamWriter::new(mmap, out_recv).flush_loop();
        });

        compositor.run(should_exit.clone());
    });

    ()
}
