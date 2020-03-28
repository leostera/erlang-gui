use erletf::Eterm;
use winit::event::{
    ElementState, Event, KeyboardInput, MouseButton, ScanCode, VirtualKeyCode, WindowEvent,
};

use crate::beam_io::{AsErlangTerm, TypeAsAtom};
use crate::command::CommandOut;

impl AsErlangTerm for ScanCode {
    fn as_erlang_term(&self) -> Eterm {
        Eterm::Integer(*self as i32)
    }
}

impl AsErlangTerm for Option<VirtualKeyCode> {
    fn as_erlang_term(&self) -> Eterm {
        match self {
            None => atom! { "none" },
            Some(vkc) => atom! { match vkc {
                  VirtualKeyCode::A => "A",
                  VirtualKeyCode::AbntC1 => "AbntC1",
                  VirtualKeyCode::AbntC2 => "AbntC2",
                  VirtualKeyCode::Add => "Add",
                  VirtualKeyCode::Apostrophe => "Apostrophe",
                  VirtualKeyCode::Apps => "Apps",
                  VirtualKeyCode::At => "At",
                  VirtualKeyCode::Ax => "Ax",
                  VirtualKeyCode::B => "B",
                  VirtualKeyCode::Back => "Back",
                  VirtualKeyCode::Backslash => "Backslash",
                  VirtualKeyCode::C => "C",
                  VirtualKeyCode::Calculator => "Calculator",
                  VirtualKeyCode::Capital => "Capital",
                  VirtualKeyCode::Caret => "Caret",
                  VirtualKeyCode::Colon => "Colon",
                  VirtualKeyCode::Comma => "Comma",
                  VirtualKeyCode::Compose => "Compose",
                  VirtualKeyCode::Convert => "Convert",
                  VirtualKeyCode::Copy => "Copy",
                  VirtualKeyCode::Cut => "Cut",
                  VirtualKeyCode::D => "D",
                  VirtualKeyCode::Decimal => "Decimal",
                  VirtualKeyCode::Delete => "Delete",
                  VirtualKeyCode::Divide => "Divide",
                  VirtualKeyCode::Down => "Down",
                  VirtualKeyCode::E => "E",
                  VirtualKeyCode::End => "End",
                  VirtualKeyCode::Equals => "Equals",
                  VirtualKeyCode::Escape => "Escape",
                  VirtualKeyCode::F => "F",
                  VirtualKeyCode::F1 => "F1",
                  VirtualKeyCode::F10 => "F10",
                  VirtualKeyCode::F11 => "F11",
                  VirtualKeyCode::F12 => "F12",
                  VirtualKeyCode::F13 => "F13",
                  VirtualKeyCode::F14 => "F14",
                  VirtualKeyCode::F15 => "F15",
                  VirtualKeyCode::F16 => "F16",
                  VirtualKeyCode::F17 => "F17",
                  VirtualKeyCode::F18 => "F18",
                  VirtualKeyCode::F19 => "F19",
                  VirtualKeyCode::F2 => "F2",
                  VirtualKeyCode::F20 => "F20",
                  VirtualKeyCode::F21 => "F21",
                  VirtualKeyCode::F22 => "F22",
                  VirtualKeyCode::F23 => "F23",
                  VirtualKeyCode::F24 => "F24",
                  VirtualKeyCode::F3 => "F3",
                  VirtualKeyCode::F4 => "F4",
                  VirtualKeyCode::F5 => "F5",
                  VirtualKeyCode::F6 => "F6",
                  VirtualKeyCode::F7 => "F7",
                  VirtualKeyCode::F8 => "F8",
                  VirtualKeyCode::F9 => "F9",
                  VirtualKeyCode::G => "G",
                  VirtualKeyCode::Grave => "Grave",
                  VirtualKeyCode::H => "H",
                  VirtualKeyCode::Home => "Home",
                  VirtualKeyCode::I => "I",
                  VirtualKeyCode::Insert => "Insert",
                  VirtualKeyCode::J => "J",
                  VirtualKeyCode::K => "K",
                  VirtualKeyCode::Kana => "Kana",
                  VirtualKeyCode::Kanji => "Kanji",
                  VirtualKeyCode::Key0 => "Key0",
                  VirtualKeyCode::Key1 => "Key1",
                  VirtualKeyCode::Key2 => "Key2",
                  VirtualKeyCode::Key3 => "Key3",
                  VirtualKeyCode::Key4 => "Key4",
                  VirtualKeyCode::Key5 => "Key5",
                  VirtualKeyCode::Key6 => "Key6",
                  VirtualKeyCode::Key7 => "Key7",
                  VirtualKeyCode::Key8 => "Key8",
                  VirtualKeyCode::Key9 => "Key9",
                  VirtualKeyCode::L => "L",
                  VirtualKeyCode::LAlt => "LAlt",
                  VirtualKeyCode::LBracket => "LBracket",
                  VirtualKeyCode::LControl => "LControl",
                  VirtualKeyCode::LShift => "LShift",
                  VirtualKeyCode::LWin => "LWin",
                  VirtualKeyCode::Left => "Left",
                  VirtualKeyCode::M => "M",
                  VirtualKeyCode::Mail => "Mail",
                  VirtualKeyCode::MediaSelect => "MediaSelect",
                  VirtualKeyCode::MediaStop => "MediaStop",
                  VirtualKeyCode::Minus => "Minus",
                  VirtualKeyCode::Multiply => "Multiply",
                  VirtualKeyCode::Mute => "Mute",
                  VirtualKeyCode::MyComputer => "MyComputer",
                  VirtualKeyCode::N => "N",
                  VirtualKeyCode::NavigateBackward => "NavigateBackward",
                  VirtualKeyCode::NavigateForward => "NavigateForward",
                  VirtualKeyCode::NextTrack => "NextTrack",
                  VirtualKeyCode::NoConvert => "NoConvert",
                  VirtualKeyCode::Numlock => "Numlock",
                  VirtualKeyCode::Numpad0 => "Numpad0",
                  VirtualKeyCode::Numpad1 => "Numpad1",
                  VirtualKeyCode::Numpad2 => "Numpad2",
                  VirtualKeyCode::Numpad3 => "Numpad3",
                  VirtualKeyCode::Numpad4 => "Numpad4",
                  VirtualKeyCode::Numpad5 => "Numpad5",
                  VirtualKeyCode::Numpad6 => "Numpad6",
                  VirtualKeyCode::Numpad7 => "Numpad7",
                  VirtualKeyCode::Numpad8 => "Numpad8",
                  VirtualKeyCode::Numpad9 => "Numpad9",
                  VirtualKeyCode::NumpadComma => "NumpadComma",
                  VirtualKeyCode::NumpadEnter => "NumpadEnter",
                  VirtualKeyCode::NumpadEquals => "NumpadEquals",
                  VirtualKeyCode::O => "O",
                  VirtualKeyCode::OEM102 => "OEM102",
                  VirtualKeyCode::P => "P",
                  VirtualKeyCode::PageDown => "PageDown",
                  VirtualKeyCode::PageUp => "PageUp",
                  VirtualKeyCode::Paste => "Paste",
                  VirtualKeyCode::Pause => "Pause",
                  VirtualKeyCode::Period => "Period",
                  VirtualKeyCode::PlayPause => "PlayPause",
                  VirtualKeyCode::Power => "Power",
                  VirtualKeyCode::PrevTrack => "PrevTrack",
                  VirtualKeyCode::Q => "Q",
                  VirtualKeyCode::R => "R",
                  VirtualKeyCode::RAlt => "RAlt",
                  VirtualKeyCode::RBracket => "RBracket",
                  VirtualKeyCode::RControl => "RControl",
                  VirtualKeyCode::RShift => "RShift",
                  VirtualKeyCode::RWin => "RWin",
                  VirtualKeyCode::Return => "Return",
                  VirtualKeyCode::Right => "Right",
                  VirtualKeyCode::S => "S",
                  VirtualKeyCode::Scroll => "Scroll",
                  VirtualKeyCode::Semicolon => "Semicolon",
                  VirtualKeyCode::Slash => "Slash",
                  VirtualKeyCode::Sleep => "Sleep",
                  VirtualKeyCode::Snapshot => "Snapshot",
                  VirtualKeyCode::Space => "Space",
                  VirtualKeyCode::Stop => "Stop",
                  VirtualKeyCode::Subtract => "Subtract",
                  VirtualKeyCode::Sysrq => "Sysrq",
                  VirtualKeyCode::T => "T",
                  VirtualKeyCode::Tab => "Tab",
                  VirtualKeyCode::U => "U",
                  VirtualKeyCode::Underline => "Underline",
                  VirtualKeyCode::Unlabeled => "Unlabeled",
                  VirtualKeyCode::Up => "Up",
                  VirtualKeyCode::V => "V",
                  VirtualKeyCode::VolumeDown => "VolumeDown",
                  VirtualKeyCode::VolumeUp => "VolumeUp",
                  VirtualKeyCode::W => "W",
                  VirtualKeyCode::Wake => "Wake",
                  VirtualKeyCode::WebBack => "WebBack",
                  VirtualKeyCode::WebFavorites => "WebFavorites",
                  VirtualKeyCode::WebForward => "WebForward",
                  VirtualKeyCode::WebHome => "WebHome",
                  VirtualKeyCode::WebRefresh => "WebRefresh",
                  VirtualKeyCode::WebSearch => "WebSearch",
                  VirtualKeyCode::WebStop => "WebStop",
                  VirtualKeyCode::X => "X",
                  VirtualKeyCode::Y => "Y",
                  VirtualKeyCode::Yen => "Yen",
                  VirtualKeyCode::Z => "Z",
              }
            },
        }
    }
}

impl AsErlangTerm for MouseButton {
    fn as_erlang_term(&self) -> Eterm {
        match self {
            MouseButton::Left => atom! { "left" },
            MouseButton::Right => atom! { "right" },
            MouseButton::Middle => atom! { "middle" },
            MouseButton::Other(i) => tuple! { atom! { "other" }, Eterm::Integer(*i as i32) },
        }
    }
}

impl AsErlangTerm for ElementState {
    fn as_erlang_term(&self) -> Eterm {
        match self {
            ElementState::Pressed => atom! { "pressed" },
            ElementState::Released => atom! { "released" },
        }
    }
}

impl AsErlangTerm for KeyboardInput {
    fn as_erlang_term(&self) -> Eterm {
        let r = proplist! {
            {"scancode", self.scancode.as_erlang_term()},
            {"state", self.state.as_erlang_term()},
            {"virtual_keycode", self.virtual_keycode.as_erlang_term()},
        };
        r
    }
}

impl<'a> TypeAsAtom for WindowEvent<'a> {
    fn type_as_atom(&self) -> Eterm {
        let name = match self {
            WindowEvent::Resized(_size) => "resized",
            WindowEvent::Moved(_) => "moved",
            WindowEvent::CloseRequested => "close_requested",
            WindowEvent::Destroyed => "destroyed",
            WindowEvent::KeyboardInput { .. } => "keyboard_input",
            WindowEvent::CursorMoved { .. } => "cursor_moved",
            WindowEvent::CursorEntered { .. } => "cursor_entered",
            WindowEvent::CursorLeft { .. } => "cursor_left",
            WindowEvent::MouseWheel { .. } => "mouse_wheel",
            WindowEvent::MouseInput { .. } => "mouse_input",
            WindowEvent::DroppedFile(_) => "dropped_file",
            WindowEvent::HoveredFile(_) => "hovered_file",
            WindowEvent::HoveredFileCancelled => "hovered_file_cancelled",
            WindowEvent::ReceivedCharacter(_) => "received_character",
            WindowEvent::Focused(_) => "focused",
            WindowEvent::TouchpadPressure { .. } => "touchpad_pressure",
            WindowEvent::AxisMotion { .. } => "axis_motion",
            WindowEvent::Touch(_) => "touch",
            WindowEvent::ScaleFactorChanged { .. } => "scale_factor_changed",
            WindowEvent::ThemeChanged(_) => "theme_changed",
        };
        atom! { name }
    }
}

impl<'a> AsErlangTerm for WindowEvent<'a> {
    fn as_erlang_term(&self) -> Eterm {
        tuple! {
            tuple! {
                atom! { "type" },
                self.type_as_atom()
            },
            match self {
                WindowEvent::Resized(size) => list! {
                    tuple! { atom! { "w" }, Eterm::Integer(size.width as i32) },
                    tuple! { atom! { "h" }, Eterm::Integer(size.height as i32) },
                },
                WindowEvent::Moved(pos) => list! {
                    tuple! { atom! { "x" }, Eterm::Integer(pos.x as i32) },
                    tuple! { atom! { "y" }, Eterm::Integer(pos.y as i32) },
                },
                WindowEvent::CloseRequested => list! {},
                WindowEvent::Destroyed => list! {},
                WindowEvent::KeyboardInput {
                    input,
                    is_synthetic,
                    ..
                } => proplist! {
                    { "kind", if *is_synthetic { atom! { "synthetic" } }
                              else { atom! { "organic" } }
                    },
                    {"input", input.as_erlang_term() },
                },
                WindowEvent::CursorMoved { position, ..  } => proplist! {
                    { "x", Eterm::Integer(position.x as i32) },
                    { "y", Eterm::Integer(position.y as i32) },
                },
                WindowEvent::AxisMotion { .. } =>  list! {} ,
                WindowEvent::CursorEntered { .. } =>  list! {} ,
                WindowEvent::CursorLeft { .. } =>  list! {} ,
                WindowEvent::DroppedFile(_) =>  list! {} ,
                WindowEvent::Focused(_) =>  list! {} ,
                WindowEvent::HoveredFile(_) =>  list! {} ,
                WindowEvent::HoveredFileCancelled =>  list! {} ,
                WindowEvent::MouseInput { state, button, ..  } => proplist! {
                    { "state",  state.as_erlang_term() },
                    { "button", button.as_erlang_term() },
                },
                WindowEvent::MouseWheel { ..  } =>  list! {} ,
                WindowEvent::ReceivedCharacter(_) =>  list! {} ,
                WindowEvent::ScaleFactorChanged { .. } =>  list! {} ,
                WindowEvent::ThemeChanged(_) => list! {},
                WindowEvent::Touch(_) =>  list! {} ,
                WindowEvent::TouchpadPressure { .. } =>  list! {} ,
            }
        }
    }
}

impl AsErlangTerm for Event<'_, ()> {
    fn as_erlang_term(&self) -> Eterm {
        match self {
            Event::WindowEvent { event, .. } => event.as_erlang_term(),
            Event::DeviceEvent { .. } => atom! { "device_event" },
            Event::Suspended => atom! { "suspended" },
            Event::Resumed => atom! { "resumed" },
            Event::NewEvents(_) => atom! { "new_events" },
            Event::UserEvent(_) => atom! { "user_events" },
            Event::MainEventsCleared { .. } => atom! { "main_events_cleared" },
            Event::RedrawRequested { .. } => atom! { "redraw_requested" },
            Event::RedrawEventsCleared => atom! { "redraw_events_cleared" },
            Event::LoopDestroyed => atom! { "loop_destroyed" },
        }
    }
}

pub fn to_command(e: Event<'_, ()>) -> Option<CommandOut> {
    match e {
        Event::LoopDestroyed => None,
        Event::MainEventsCleared { .. } => None,
        Event::NewEvents(_) => None,
        Event::RedrawEventsCleared => None,
        Event::RedrawRequested { .. } => None,
        Event::UserEvent(_) => None,
        _ => Some(CommandOut::Relay(e.as_erlang_term())),
    }
}
