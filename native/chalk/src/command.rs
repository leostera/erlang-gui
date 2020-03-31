use std::time::{SystemTime, UNIX_EPOCH};

use erletf::Eterm;

use crate::beam_io::{AsErlangTerm, FromErlangTerm};

use skia_safe::Picture;

pub enum CommandIn {
    // Echo the same term back to the BEAM
    Echo(Eterm),

    // Render a frame
    Render(Picture),
}

pub enum CommandOut {
    // Echo the same term back to the BEAM
    Echo(Eterm),

    // Relay information to the beam
    Relay(Eterm),

    // Request a frame
    RequestFrame,

    // Note that a frame was queued for rendering
    RenderQueued,
}

fn now() -> u128 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .expect("Time went backwards")
        .as_nanos()
}

impl AsErlangTerm for CommandOut {
    fn as_erlang_term(&self) -> Eterm {
        match self {
            CommandOut::Echo(term) => tuple! {
                tuple! { atom! { "ts" }, bignum! { now() } },
                tuple! { atom! { "kind" }, atom! { "echo" } },
                tuple! { atom! { "data" }, term.clone() }
            },

            CommandOut::Relay(event) => tuple! {
                tuple! { atom! { "ts" }, bignum! { now() } },
                tuple! { atom! { "kind" }, atom! { "relay" } },
                tuple! { atom! { "data" }, event.clone() }
            },

            CommandOut::RequestFrame => tuple! {
                tuple! { atom! { "ts" }, bignum! { now() } },
                tuple! { atom! { "kind" }, atom! { "command" } },
                tuple! { atom! { "command" }, atom! { "request_frame" } }
            },

            CommandOut::RenderQueued => tuple! {
                tuple! { atom! { "ts" }, bignum! { now() } },
                tuple! { atom! { "kind" }, atom! { "notice" } },
                tuple! { atom! { "data" }, atom! { "render_queued" } }
            },
        }
    }
}

impl FromErlangTerm<CommandIn> for CommandIn {
    fn from_erlang_term(term: Eterm) -> CommandIn {
        match term {
            Eterm::Tuple(cmd) if cmd.len() > 1 => {
                if cmd.len() != 3 {
                    panic!("Malformed command!")
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
                    (_, "echo", data) => CommandIn::Echo(data),
                    (_, "render", term) => match term {
                        Eterm::Binary(raw_image) => {
                            let picture = Picture::from_bytes(&raw_image.to_vec()).unwrap();
                            CommandIn::Render(picture)
                        },
                        _ => panic!("Malformed render command, are you sure you're sending a binary string?")
                    },
                    _ => panic!("We can only turn echo and render commands from Erlang"),
                }
            }
            term => panic!(
                "Malformed command! Are you sure you're sending the right tuples?\n{:?}",
                term
            ),
        }
    }
}
