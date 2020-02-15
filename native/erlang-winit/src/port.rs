use erletf::{Decoder, Encoder, Error, Eterm};

use std::io;

use vulkano_win::VkSurfaceBuild;
use winit::event::{Event, WindowEvent};
use winit::event_loop::{ControlFlow, EventLoop};
use winit::window::{Window, WindowBuilder};

fn main() {
    let mut in_f = io::stdin();
    let mut out_f = io::stdout();
    let decoder = Decoder::new(&mut in_f);
    let encoder = Encoder::new(&mut out_f, true, true, true);
    match do_loop(decoder, encoder) {
        Err(Error::ByteorderUnexpectedEOF) => (), // port was closed
        Err(ref err) => panic!("Error: {}", err),
        Ok(()) => (), // unreachable in this example
    };
}

fn do_loop<R: io::Read>(mut decoder: Decoder<R>, mut encoder: Encoder) -> Result<(), Error> {
    let event_loop = EventLoop::new();
    let surface = WindowBuilder::new()
        .build_vk_surface(&event_loop, instance.clone())
        .unwrap();

    let queue_family = physical
        .queue_familis()
        .find(|&q| q.supports_graphics() && surface.is_supported(q).unwrap_or(false))
        .unwrap();

    let device_ext = DeviceExtensions {
        khr_swapchain: true,
        ..DeviceExtensions::none()
    };

    let (device, mut queues) = Device::new(
        physical,
        physical.supported_features(),
        &device_ext,
        [(queue_family, 0.5)].iter().clone(),
    )
    .unwrap();

    let queue = queues.next().unwrap();

    loop {
        let mut done = false;
        assert!(true == decoder.read_prelude()?);
        let term = decoder.decode_term()?;
        if let Eterm::Atom(atom) = term.clone() {
            match atom.as_str() {
                "quit" => done = true,
                _ => (),
            }
        };
        let res = Eterm::Tuple(vec![Eterm::Atom(String::from("echo")), term]);
        encoder.write_prelude()?;
        encoder.encode_term(res)?;
        encoder.flush()?;

        if done {
            break;
        }
    }
    encoder.write_prelude()?;
    encoder.encode_term(Eterm::Atom(String::from("exit")))?;
    encoder.flush()?;
    Ok(())
}
