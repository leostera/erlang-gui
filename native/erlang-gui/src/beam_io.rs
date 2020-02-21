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
    match &cmd[1] {
        Eterm::Atom(tag) => match tag.as_str() {
            "echo" => Some(Eterm::Tuple(vec![
                Eterm::Atom(String::from("echo")),
                Eterm::Tuple(cmd),
            ])),
            "render" => {
                let image = cmd[2].clone();

                match image {
                    Eterm::Binary(raw_image) => render_queue.push(raw_image),
                    _ => (),
                };

                Some(Eterm::Tuple(vec![Eterm::Atom(String::from(
                    "render_queued",
                ))]))
            }
            "relay" => Some(Eterm::Tuple(cmd)),
            _ => Some(Eterm::Tuple(vec![Eterm::Atom(String::from(
                "unknown_command",
            ))])),
        },
        _ => None,
    }
}
