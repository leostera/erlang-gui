extern crate crossbeam;

use std::io;

use crossbeam::queue::SegQueue;

use erletf::{Decoder, Encoder, Error, Eterm};

pub fn reader(stdin: &mut io::Stdin, command_queue: &SegQueue<Eterm>) -> Result<(), Error> {
    let mut decoder = Decoder::new(stdin);
    loop {
        assert!(true == decoder.read_prelude()?);
        let term = decoder.decode_term()?;
        command_queue.push(term.clone());
    }
}

pub fn command_processor(
    stdout: &mut io::Stdout,
    command_queue: &SegQueue<Eterm>,
    render_queue: &SegQueue<Vec<u8>>,
) -> Result<(), Error> {
    let mut encoder = Encoder::new(stdout, true, true, true);
    loop {
        let output = match command_queue.pop() {
            Ok(Eterm::Tuple(tup)) if tup.len() > 1 => handle_command(tup.clone(), render_queue),
            _ => None,
        };
        match output {
            Some(cmd) => {
                encoder.write_prelude()?;
                encoder.encode_term(cmd)?;
                encoder.flush()?
            }
            _ => (),
        }
    }
}

fn handle_command(cmd: Vec<Eterm>, render_queue: &SegQueue<Vec<u8>>) -> Option<Eterm> {
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
        (_, "render", frame) => {
            match frame {
                Eterm::Binary(raw_image) => render_queue.push(raw_image.to_vec()),
                _ => (),
            };
            Some(tuple! { atom! { "render_queued" } })
        }
        _ => None,
    }
}
