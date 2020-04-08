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
                  VirtualKeyCode::A => "a",
                  VirtualKeyCode::AbntC1 => "abnt_c1",
                  VirtualKeyCode::AbntC2 => "Abnt_c2",
                  VirtualKeyCode::Add => "add",
                  VirtualKeyCode::Apostrophe => "apostrophe",
                  VirtualKeyCode::Apps => "apps",
                  VirtualKeyCode::At => "at",
                  VirtualKeyCode::Ax => "ax",
                  VirtualKeyCode::B => "b",
                  VirtualKeyCode::Back => "back",
                  VirtualKeyCode::Backslash => "backslash",
                  VirtualKeyCode::C => "c",
                  VirtualKeyCode::Calculator => "calculator",
                  VirtualKeyCode::Capital => "capital",
                  VirtualKeyCode::Caret => "caret",
                  VirtualKeyCode::Colon => "colon",
                  VirtualKeyCode::Comma => "comma",
                  VirtualKeyCode::Compose => "compose",
                  VirtualKeyCode::Convert => "convert",
                  VirtualKeyCode::Copy => "copy",
                  VirtualKeyCode::Cut => "cut",
                  VirtualKeyCode::D => "d",
                  VirtualKeyCode::Decimal => "decimal",
                  VirtualKeyCode::Delete => "delete",
                  VirtualKeyCode::Divide => "divide",
                  VirtualKeyCode::Down => "down",
                  VirtualKeyCode::E => "e",
                  VirtualKeyCode::End => "end",
                  VirtualKeyCode::Equals => "equals",
                  VirtualKeyCode::Escape => "escape",
                  VirtualKeyCode::F => "f",
                  VirtualKeyCode::F1 => "f1",
                  VirtualKeyCode::F10 => "f10",
                  VirtualKeyCode::F11 => "f11",
                  VirtualKeyCode::F12 => "f12",
                  VirtualKeyCode::F13 => "f13",
                  VirtualKeyCode::F14 => "f14",
                  VirtualKeyCode::F15 => "f15",
                  VirtualKeyCode::F16 => "f16",
                  VirtualKeyCode::F17 => "f17",
                  VirtualKeyCode::F18 => "f18",
                  VirtualKeyCode::F19 => "f19",
                  VirtualKeyCode::F2 => "f2",
                  VirtualKeyCode::F20 => "f20",
                  VirtualKeyCode::F21 => "f21",
                  VirtualKeyCode::F22 => "f22",
                  VirtualKeyCode::F23 => "f23",
                  VirtualKeyCode::F24 => "f24",
                  VirtualKeyCode::F3 => "f3",
                  VirtualKeyCode::F4 => "f4",
                  VirtualKeyCode::F5 => "f5",
                  VirtualKeyCode::F6 => "f6",
                  VirtualKeyCode::F7 => "f7",
                  VirtualKeyCode::F8 => "f8",
                  VirtualKeyCode::F9 => "f9",
                  VirtualKeyCode::G => "g",
                  VirtualKeyCode::Grave => "grave",
                  VirtualKeyCode::H => "h",
                  VirtualKeyCode::Home => "home",
                  VirtualKeyCode::I => "i",
                  VirtualKeyCode::Insert => "insert",
                  VirtualKeyCode::J => "j",
                  VirtualKeyCode::K => "k",
                  VirtualKeyCode::Kana => "kana",
                  VirtualKeyCode::Kanji => "kanji",
                  VirtualKeyCode::Key0 => "key_0",
                  VirtualKeyCode::Key1 => "key_1",
                  VirtualKeyCode::Key2 => "key_2",
                  VirtualKeyCode::Key3 => "key_3",
                  VirtualKeyCode::Key4 => "key_4",
                  VirtualKeyCode::Key5 => "key_5",
                  VirtualKeyCode::Key6 => "key_6",
                  VirtualKeyCode::Key7 => "key_7",
                  VirtualKeyCode::Key8 => "key_8",
                  VirtualKeyCode::Key9 => "key_9",
                  VirtualKeyCode::L => "l",
                  VirtualKeyCode::LAlt => "left_alt",
                  VirtualKeyCode::LBracket => "left_bracket",
                  VirtualKeyCode::LControl => "left_control",
                  VirtualKeyCode::LShift => "left_shift",
                  VirtualKeyCode::LWin => "left_super",
                  VirtualKeyCode::Left => "left",
                  VirtualKeyCode::M => "m",
                  VirtualKeyCode::Mail => "mail",
                  VirtualKeyCode::MediaSelect => "media_select",
                  VirtualKeyCode::MediaStop => "media_stop",
                  VirtualKeyCode::Minus => "minus",
                  VirtualKeyCode::Multiply => "multiply",
                  VirtualKeyCode::Mute => "mute",
                  VirtualKeyCode::MyComputer => "my_computer",
                  VirtualKeyCode::N => "n",
                  VirtualKeyCode::NavigateBackward => "navigate_backward",
                  VirtualKeyCode::NavigateForward => "navigate_forward",
                  VirtualKeyCode::NextTrack => "next_track",
                  VirtualKeyCode::NoConvert => "no_convert",
                  VirtualKeyCode::Numlock => "numlock",
                  VirtualKeyCode::Numpad0 => "numpad_0",
                  VirtualKeyCode::Numpad1 => "numpad_1",
                  VirtualKeyCode::Numpad2 => "numpad_2",
                  VirtualKeyCode::Numpad3 => "numpad_3",
                  VirtualKeyCode::Numpad4 => "numpad_4",
                  VirtualKeyCode::Numpad5 => "numpad_5",
                  VirtualKeyCode::Numpad6 => "numpad_6",
                  VirtualKeyCode::Numpad7 => "numpad_7",
                  VirtualKeyCode::Numpad8 => "numpad_8",
                  VirtualKeyCode::Numpad9 => "numpad_9",
                  VirtualKeyCode::NumpadComma => "numpad_comma",
                  VirtualKeyCode::NumpadEnter => "numpad_enter",
                  VirtualKeyCode::NumpadEquals => "numpad_equals",
                  VirtualKeyCode::O => "o",
                  VirtualKeyCode::OEM102 => "oem_102",
                  VirtualKeyCode::P => "p",
                  VirtualKeyCode::PageDown => "page_down",
                  VirtualKeyCode::PageUp => "page_up",
                  VirtualKeyCode::Paste => "paste",
                  VirtualKeyCode::Pause => "pause",
                  VirtualKeyCode::Period => "period",
                  VirtualKeyCode::PlayPause => "play_pause",
                  VirtualKeyCode::Power => "power",
                  VirtualKeyCode::PrevTrack => "prev_track",
                  VirtualKeyCode::Q => "q",
                  VirtualKeyCode::R => "r",
                  VirtualKeyCode::RAlt => "right_alt",
                  VirtualKeyCode::RBracket => "right_bracket",
                  VirtualKeyCode::RControl => "right_control",
                  VirtualKeyCode::RShift => "right_shift",
                  VirtualKeyCode::RWin => "right_super",
                  VirtualKeyCode::Return => "return",
                  VirtualKeyCode::Right => "right",
                  VirtualKeyCode::S => "s",
                  VirtualKeyCode::Scroll => "scroll",
                  VirtualKeyCode::Semicolon => "semicolon",
                  VirtualKeyCode::Slash => "slash",
                  VirtualKeyCode::Sleep => "sleep",
                  VirtualKeyCode::Snapshot => "snapshot",
                  VirtualKeyCode::Space => "space",
                  VirtualKeyCode::Stop => "stop",
                  VirtualKeyCode::Subtract => "subtract",
                  VirtualKeyCode::Sysrq => "system_request",
                  VirtualKeyCode::T => "t",
                  VirtualKeyCode::Tab => "tab",
                  VirtualKeyCode::U => "u",
                  VirtualKeyCode::Underline => "underline",
                  VirtualKeyCode::Unlabeled => "unlabeled",
                  VirtualKeyCode::Up => "up",
                  VirtualKeyCode::V => "v",
                  VirtualKeyCode::VolumeDown => "volume_down",
                  VirtualKeyCode::VolumeUp => "volume_up",
                  VirtualKeyCode::W => "w",
                  VirtualKeyCode::Wake => "wake",
                  VirtualKeyCode::WebBack => "web_back",
                  VirtualKeyCode::WebFavorites => "web_favorites",
                  VirtualKeyCode::WebForward => "web_forward",
                  VirtualKeyCode::WebHome => "web_home",
                  VirtualKeyCode::WebRefresh => "web_refresh",
                  VirtualKeyCode::WebSearch => "web_search",
                  VirtualKeyCode::WebStop => "web_stop",
                  VirtualKeyCode::X => "x",
                  VirtualKeyCode::Y => "y",
                  VirtualKeyCode::Yen => "yen",
                  VirtualKeyCode::Z => "z",
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
