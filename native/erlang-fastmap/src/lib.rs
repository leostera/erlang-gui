#[macro_use]
extern crate rustler;

use std::collections::BTreeMap;

use rustler::{
    types::binary::{Binary, OwnedBinary},
    Encoder, Env, Error, ResourceArc, Term,
};
use std::sync::RwLock;

struct FastMap {
    data: RwLock<BTreeMap<(u32, u32, u32), Vec<u8>>>,
}

mod atoms {
    rustler_atoms! {
        atom ok;
        atom error;
        atom none;
    }
}

rustler::rustler_export_nifs! {
    "fastmap_native",
    [
        ("new", 0, fastmap_new),
        ("insert", 3, fastmap_insert),
        ("as_list", 1, fastmap_as_list),
    ],
    Some(on_init)
}

fn on_init<'a>(env: Env<'a>, _load_info: Term<'a>) -> bool {
    resource_struct_init!(FastMap, env);
    true
}

fn fastmap_new<'a>(env: Env<'a>, _args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let fastmap_struct = FastMap {
        data: RwLock::new(BTreeMap::new()),
    };

    Ok(ResourceArc::new(fastmap_struct).encode(env))
}

fn fastmap_as_list<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let resource: ResourceArc<FastMap> = args[0].decode()?;

    let res = match resource.data.read() {
        Ok(map) => map
            .keys()
            .cloned()
            .map(|k| {
                let v = map.get(&k).unwrap();
                let mut bin = OwnedBinary::new(v.len()).unwrap();
                bin.copy_from_slice(&v);
                (k, Binary::from_owned(bin, env))
            })
            .collect(),
        _ => vec![],
    };

    Ok(res.encode(env))
}

fn fastmap_insert<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let map: ResourceArc<FastMap> = args[0].decode()?;
    let key: (u32, u32, u32) = args[1].decode()?;
    let value: Binary = args[2].decode()?;

    match map.data.write() {
        Ok(mut m) => {
            m.insert(key, value.to_vec());
        }
        _ => (),
    };

    Ok(map.encode(env))
}
