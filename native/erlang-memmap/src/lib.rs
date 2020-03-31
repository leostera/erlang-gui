#[macro_use]
extern crate rustler;

use std::fs::{File, OpenOptions};
use std::io::Write;
use std::path::PathBuf;
use std::sync::RwLock;

use memmap::{Mmap, MmapMut, MmapOptions};

use rustler::{
    types::binary::{Binary, OwnedBinary},
    Encoder, Env, Error, ResourceArc, Term,
};

struct ReadOnlyMmap {
    data: RwLock<Mmap>,
}

struct WriteMmap {
    data: RwLock<MmapMut>,
    file: RwLock<File>,
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
    "memmap_native",
    [
        ("open_read", 2, open_read),
        ("open_write", 2, open_write),
        ("read", 3, read),
        ("write", 3, write),
    ],
    Some(on_init)
}

fn on_init<'a>(env: Env<'a>, _load_info: Term<'a>) -> bool {
    resource_struct_init!(ReadOnlyMmap, env);
    resource_struct_init!(WriteMmap, env);
    true
}

fn open_write<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let path: String = args[0].decode()?;
    let len: u64 = args[1].decode()?;

    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(PathBuf::from(path))
        .unwrap();

    file.set_len(len).unwrap();

    let mmap = unsafe { MmapMut::map_mut(&file).unwrap() };

    let resource = WriteMmap {
        data: RwLock::new(mmap),
        file: RwLock::new(file),
    };

    Ok(ResourceArc::new(resource).encode(env))
}

fn open_read<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let path: String = args[0].decode()?;
    let len: u64 = args[1].decode()?;

    let file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(PathBuf::from(path))
        .unwrap();

    file.set_len(len).unwrap();

    let mmap = unsafe { Mmap::map(&file).unwrap() };

    let resource = ReadOnlyMmap {
        data: RwLock::new(mmap),
    };

    Ok(ResourceArc::new(resource).encode(env))
}

fn read<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let resource: ResourceArc<ReadOnlyMmap> = args[0].decode()?;
    let size: usize = args[1].decode()?;
    let offset: usize = args[2].decode()?;

    let mmap = &*resource.data.read().unwrap();
    let data = &mmap[offset..offset + size];

    let mut bin = OwnedBinary::new(data.len()).unwrap();
    bin.copy_from_slice(&data);

    Ok(Binary::from_owned(bin, env).encode(env))
}

fn write<'a>(env: Env<'a>, args: &[Term<'a>]) -> Result<Term<'a>, Error> {
    let resource: ResourceArc<WriteMmap> = args[0].decode()?;
    let data: Binary = args[1].decode()?;
    let offset: u64 = args[2].decode()?;

    match offset {
        0 => {
            let mmap = &mut *resource.data.write().unwrap();
            (&mut mmap[..]).write_all(&data.as_slice()).unwrap();
            &mmap.flush().unwrap();
        }
        _ => {
            let file = resource.file.write().unwrap();
            let mut mmap = unsafe { MmapOptions::new().offset(offset).map_mut(&file).unwrap() };
            (&mut mmap[..]).write_all(&data.as_slice()).unwrap();
            &mmap.flush().unwrap();
        }
    };

    Ok(offset.encode(env))
}
