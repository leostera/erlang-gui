#[macro_use]
extern crate rustler;

use rustler::{Encoder, Env, Error, ResourceArc, Term};
use std::sync::RwLock;

struct Buffer {
    data: RwLock<Vec<u8>>,
}

mod atoms {
    rustler_atoms! {
        atom ok;
        atom error;
        //atom __true__ = "true";
        //atom __false__ = "false";
    }
}

rustler::rustler_export_nifs! {
    "binbuf",
    [
        ("new", 1, buffer_new),
        ("get", 2, buffer_get),
        ("set", 3, buffer_set),
    ],
    Some(on_init)
}

fn on_init<'a>(env: Env<'a>, _load_info: Term<'a>) -> bool {
    resource_struct_init!(Buffer, env);
    true
}

fn buffer_new<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let buffer_size: usize = args[0].decode()?;
    let mut buffer = Vec::with_capacity(buffer_size);
    for _i in 0..buffer_size {
        buffer.push(0);
    }
    let buffer_struct = Buffer {
        data: RwLock::new(buffer),
    };

    Ok(ResourceArc::new(buffer_struct).encode(env))
}

fn buffer_get<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let buffer: ResourceArc<Buffer> = args[0].decode()?;
    let offset: usize = args[1].decode()?;

    let res = match buffer.data.read() {
        Ok(buf) => buf[offset],
        _ => 0,
    };

    Ok(res.encode(env))
}

fn buffer_set<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let buffer: ResourceArc<Buffer> = args[0].decode()?;
    let offset: usize = args[1].decode()?;
    let byte: u8 = args[2].decode()?;

    match buffer.data.write() {
        Ok(mut buf) => buf[offset] = byte,
        _ => (),
    };

    Ok(byte.encode(env))
}
