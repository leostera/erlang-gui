extern crate crossbeam;

use std::thread;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use crossbeam::queue::ArrayQueue;
use erletf::{Decoder, Encoder, Eterm};

#[macro_export]
macro_rules! number {
    ($n:expr) => {
        Eterm::Integer($n as i32)
    };
}

#[macro_export]
macro_rules! bignum {
    ($n:expr) => {
        Eterm::BigNum(num::bigint::BigInt::from($n))
    };
}

#[macro_export]
macro_rules! atom {
    ($atom:expr) => {
        Eterm::Atom(String::from($atom))
    };
}

#[macro_export]
macro_rules! list {
    () => {
        Eterm::List(vec![Eterm::Nil])
    };
    // Strip trailing comma!
    ($( $erl_list_elem:expr ),+,) => {
        list!($( $erl_list_elem ),*);
    };
    ($( $erl_list_elem:expr ),*) => {
        Eterm::List(vec![
            $($erl_list_elem,)*
            Eterm::Nil
        ])
    };
}

#[macro_export]
macro_rules! tuple {
    // Single element tuple
    ($erl_tuple_elem:expr) => {
        Eterm::Tuple(vec![ $erl_tuple_elem ])
    };
    // Strip trailing comma!
    ($( $erl_tuple_elem:expr ),+,) => {
        tuple!({$( $erl_tuple_elem ),*});
    };
    ($( $erl_tuple_elem:expr ),*) => {
        Eterm::Tuple(vec![ $($erl_tuple_elem,)* ])
    };
}

#[macro_export]
macro_rules! proplist {
    ($({$key:expr,$val:expr},)*) => {
        list! { $( tuple! { atom! { $key }, $val }, )* }
    };
}

pub trait AsErlangTerm {
    fn as_erlang_term(&self) -> Eterm;
}

pub trait FromErlangTerm<T> {
    fn from_erlang_term(t: Eterm) -> T;
}

pub trait TypeAsAtom {
    fn type_as_atom(&self) -> Eterm;
}

pub struct BeamWriter<'a, T> {
    encoder: Encoder<'a>,
    // queue: &'a ArrayQueue<T>,
    queue: std::sync::mpsc::Receiver<T>,
}

impl<'a, T: AsErlangTerm> BeamWriter<'a, T> {
    pub fn new(
        stdout: &'a mut std::io::Stdout,
        // queue: &'a ArrayQueue<T>
        queue: std::sync::mpsc::Receiver<T>,
    ) -> BeamWriter<'a, T> {
        BeamWriter {
            encoder: Encoder::new(stdout, true, true, true),
            queue: queue,
        }
    }

    pub fn send(&mut self, value: Eterm) -> () {
        self.encoder
            .write_prelude()
            .expect("Encoder failed to write communication prelude");
        self.encoder
            .encode_term(tuple! { bignum! { now() }, value })
            .expect("Encoder could not encode term");
        self.encoder.flush().expect("Encoder could not flush");
    }

    pub fn flush_loop(&mut self) -> () {
        loop {
            match self.queue.recv() {
                Ok(t) => self.send(t.as_erlang_term()),
                _ => (),
            };
        }
    }
}

pub struct BeamReader<'a, T: FromErlangTerm<T> + 'static> {
    decoder: Decoder<'a, std::io::Stdin>,
    queue: &'a ArrayQueue<T>,
}

impl<'a, T: FromErlangTerm<T>> BeamReader<'a, T> {
    pub fn new(stdin: &'a mut std::io::Stdin, queue: &'a ArrayQueue<T>) -> BeamReader<'a, T> {
        BeamReader {
            decoder: Decoder::new(stdin),
            queue: queue,
        }
    }

    pub fn read(&mut self) -> () {
        match self.decoder.read_prelude() {
            Ok(_) => {
                let term = self.decoder.decode_term().expect(
                    "Could not decode Erlang term, are you sure you sent it in binary form?",
                );
                let value: T = T::from_erlang_term(term);
                self.queue.push(value);
            }
            Err(_) => std::process::exit(1),
        }
    }

    pub fn read_loop(&mut self) -> () {
        loop {
            self.read();
            thread::sleep(Duration::from_millis(1));
        }
    }
}

fn now() -> u128 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards")
        .as_nanos()
}
