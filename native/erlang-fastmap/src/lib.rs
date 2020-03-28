#[macro_use]
extern crate rustler;

/*
use std::collections::HashMap;

use rustler::{Encoder, Env, Error, ResourceArc, Term};
use std::sync::RwLock;

struct FastMap {
    data: RwLock<HashMap<Term, Term>>,
}

mod atoms {
    rustler_atoms! {
        atom ok;
        atom error;
        atom none;
        //atom __true__ = "true";
        //atom __false__ = "false";
    }
}

rustler::rustler_export_nifs! {
    "fastmap",
    [
        ("new", 1, fastmap_new),
        ("get", 2, fastmap_get),
        ("set", 3, fastmap_set),
    ],
    Some(on_init)
}

fn on_init<'a>(env: Env<'a>, _load_info: Term<'a>) -> bool {
    resource_struct_init!(FastMap, env);
    true
}

fn fastmap_new<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let fastmap_size: usize = args[0].decode()?;
    let mut map = HashMap::new();
    let fastmap_struct = FastMap {
        data: RwLock::new(map),
    };

    Ok(ResourceArc::new(fastmap_struct).encode(env))
}

fn fastmap_get<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let map: ResourceArc<FastMap> = args[0].decode()?;
    let key: Term = args[1].decode()?;

    let res = match map.data.read() {
        Ok(m) => match m.get(key) {
            Some(term) => term
            None => atoms::none()
        },
        _ => atoms::none(),
    };

    Ok(res.encode(env))
}

fn fastmap_set<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let map: ResourceArc<FastMap> = args[0].decode()?;
    let key: Term<'a> = args[1].decode()?;
    let value: Term<'a> = args[2].decode()?;

    match map.data.write() {
        Ok(mut m) => m.set(key, value),
        _ => (),
    };

    Ok(map.encode(env))
}
*/
