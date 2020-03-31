extern crate crossbeam;

use std::io::{BufReader, Read, Write};
use std::thread;
use std::time::{Duration, SystemTime, UNIX_EPOCH};

use crossbeam::queue::ArrayQueue;
use erletf::{Decoder, Encoder, Eterm};
use memmap::{Mmap, MmapMut};

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

pub struct BeamWriter<T> {
    mmap: MmapMut,
    offset: usize,
    queue: std::sync::mpsc::Receiver<T>,
}

impl<T: AsErlangTerm> BeamWriter<T> {
    pub fn new(mmap: MmapMut, queue: std::sync::mpsc::Receiver<T>) -> BeamWriter<T> {
        BeamWriter {
            mmap,
            queue,
            offset: 0,
        }
    }

    pub fn send(&mut self, value: Eterm) -> () {
        let mut buffer = Vec::new();
        let mut encoder = Encoder::new(&mut buffer, true, true, true);
        encoder
            .write_prelude()
            .expect("Encoder failed to write communication prelude");
        encoder
            .encode_term(tuple! { bignum! { now() }, value })
            .expect("Encoder could not encode term");
        encoder.flush().expect("Encoder could not flush");

        self.write(buffer);
    }

    pub fn flush_loop(&mut self) -> () {
        loop {
            match self.queue.recv() {
                Ok(t) => self.send(t.as_erlang_term()),
                _ => (),
            };
        }
    }

    fn write(&mut self, term: Vec<u8>) -> () {
        let size = term.len() + 5;
        let slice: &mut [u8] = &mut self.mmap[self.offset..self.offset + size];

        (&mut slice[0..1]).copy_from_slice(&[0]);
        (&mut slice[1..5]).copy_from_slice(&term.len().to_be_bytes()[4..8]);
        (&mut slice[5..5 + term.len()]).copy_from_slice(term.as_slice());
        slice[0] = 1;

        self.offset = self.offset + size;
    }
}

pub struct BeamReader<'a, T: FromErlangTerm<T> + 'static> {
    mmap: Mmap,
    offset: usize,
    queue: &'a ArrayQueue<T>,
}

impl<'a, T: FromErlangTerm<T>> BeamReader<'a, T> {
    pub fn new(mmap: Mmap, queue: &'a ArrayQueue<T>) -> BeamReader<'a, T> {
        BeamReader {
            mmap,
            queue,
            offset: 0,
        }
    }

    pub fn read(&mut self) -> () {
        match self.read_lock_tag() {
            0 => (),
            _ => {
                let chunk_size = self.read_chunk_size();
                let binary_term = self.read_binary_term(chunk_size);
                let mut buffer = BufReader::new(binary_term);
                let mut decoder = Decoder::new(&mut buffer);
                match decoder.read_prelude() {
                    Ok(_) => {
                        let term = decoder.decode_term().expect(
                    "Could not decode Erlang term, are you sure you sent it in binary form?",
                );
                        let value: T = T::from_erlang_term(term);
                        self.queue.push(value);
                    }
                    Err(_) => std::process::exit(1),
                };
                self.move_offset(chunk_size + 5);
            }
        }
    }

    pub fn read_loop(&mut self) -> () {
        loop {
            self.read();
        }
    }

    fn read_lock_tag(&mut self) -> u8 {
        self.mmap[self.offset] as u8
    }

    fn read_chunk_size(&mut self) -> usize {
        let mut bytes = [0; 4];
        let mut slice = &self.mmap[self.offset + 1..self.offset + 1 + 4];
        bytes.copy_from_slice(&mut slice);
        usize::from_be_bytes([0, 0, 0, 0, bytes[0], bytes[1], bytes[2], bytes[3]])
    }

    fn read_binary_term(&mut self, size: usize) -> &[u8] {
        &self.mmap[self.offset + 5..self.offset + 5 + size]
    }

    fn move_offset(&mut self, offset: usize) -> () {
        self.offset = self.offset + offset;
    }
}

fn now() -> u128 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards")
        .as_nanos()
}
