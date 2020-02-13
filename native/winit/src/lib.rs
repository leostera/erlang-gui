#[macro_use]
extern crate rustler;

use rustler::codegen_runtime::{NifReturnable, NifReturned};
use rustler::types::atom::Atom;
use rustler::{Encoder, Env, Error, Term};

use winit::{
    event::{Event, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    window::WindowBuilder,
};

mod atoms {
    rustler_atoms! {
        atom ok;
        atom error;
        //atom __true__ = "true";
        //atom __false__ = "false";
    }
}

rustler::rustler_export_nifs! {
    "Elixir.Winit.Native",
    [
        ("new_event_loop", 0, new_event_loop),
        ("run_event_loop", 1, noop),
        ("new_window", 0, noop),
        ("build_window", 1, noop),
    ],
    None
}

struct EvLoop(EventLoop<()>);

unsafe impl NifReturnable for EvLoop {
    unsafe fn as_returned(self, env: Env) -> NifReturned {
        match Atom::from_str(env, "ev_loop") {
            Ok(atom) => NifReturned::Term(atom.to_term(env).as_c_arg()),
            Err(_) => NifReturned::BadArg,
        }
    }
}

fn new_event_loop<'a>(env: Env<'a>, args: &[Term<'a>]) -> EvLoop {
    EvLoop(EventLoop::new())
}

fn noop<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<(), Error> {
    Ok(())
}

fn add<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let num1: i64 = args[0].decode()?;
    let num2: i64 = args[1].decode()?;

    Ok((atoms::ok(), num1 + num2).encode(env))
}
