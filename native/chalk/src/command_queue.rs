extern crate crossbeam;

use std::io;
use std::sync::{Arc, Mutex};

use crossbeam::queue::SegQueue;

enum Command {
    Echo(Eterm),

    Relay(RelayData),

    RequestFrame,

    Render(Frame),
}

struct CommandQueue {
    queue: &SegQueue<Command>,
    current_frame: Arc<Mutex<Option<Vec<u8>>>>,
}

impl CommandQueue {
    pub fn drain_queue(self) -> Result<(), Error> {
        loop {
            let output = match self.queue.pop() {
                Ok(Eterm::Tuple(tup)) if tup.len() > 1 => {
                    handle_command(tup.clone(), current_frame.clone())
                }
                _ => None,
            };
            match output {
                Some(cmd) => {}
                _ => (),
            };
        }
    }
}

pub fn process_commands(
    stdout: &mut io::Stdout,
    command_queue: &SegQueue<Eterm>,
    current_frame: Arc<Mutex<Option<Vec<u8>>>>,
) -> Result<(), Error> {
    let mut encoder = Encoder::new(stdout, true, true, true);
    loop {
        let output = match command_queue.pop() {
            Ok(Eterm::Tuple(tup)) if tup.len() > 1 => {
                handle_command(tup.clone(), current_frame.clone())
            }
            _ => None,
        };
        match output {
            Some(cmd) => {
                encoder.write_prelude()?;
                encoder.encode_term(cmd)?;
                encoder.flush()?
            }
            _ => (),
        };
        let ten_millis = std::time::Duration::from_millis(1);
        let now = std::time::Instant::now();
        std::thread::sleep(ten_millis);
    }
}

fn handle_command(cmd: Vec<Eterm>, current_frame: Arc<Mutex<Option<Vec<u8>>>>) -> Option<Eterm> {
    if cmd.len() != 3 {
        eprintln!("what: {:?}", cmd.clone());
        return None;
    };
    let is_ref = {
        let is_ref = match &cmd[0] {
            Eterm::Tuple(parts) => match &parts[1] {
                Eterm::Atom(is_ref) => is_ref.as_str(),
                _ => "none",
            },
            _ => "none",
        };
        is_ref != "none"
    };
    let kind = {
        match &cmd[1] {
            Eterm::Tuple(parts) => match &parts[1] {
                Eterm::Atom(kind) => kind.as_str(),
                _ => "unknown",
            },
            _ => "unknown",
        }
    };
    let data = {
        if cmd.len() > 1 {
            match &cmd[2] {
                Eterm::Tuple(parts) if parts.len() > 0 => parts[1].clone(),
                _ => atom! { "missing_data" },
            }
        } else {
            atom! { "no_data" }
        }
    };
    match (is_ref, kind, data) {
        (_, "echo", data) => Some(tuple! { atom! { "echo" }, data.clone() }),
        (_, "relay", _data) => Some(Eterm::Tuple(cmd.clone())),
        (_, "command", _data) => Some(Eterm::Tuple(cmd.clone())),
        (_, "render", term) => match term {
            Eterm::Binary(raw_image) => {
                let mut frame = current_frame.lock().unwrap();
                *frame = Some(raw_image.to_vec());
                Some(tuple! { atom! { "render_queued" } })
            }
            _ => Some(tuple! { atom! { "render_queue_failed" } }),
        },
        _ => None,
    }
}
