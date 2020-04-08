-module(chalk_event).

-compile([export_all]).

has_position({_, {{type, cursor_moved}, _}})-> true;
has_position(_)-> false.

type({_, {{type, X}, _}}) -> X;
type({_, X}) when is_atom(X) -> X.

is_valid_type(axis_motion) -> true;
is_valid_type(close_requested) -> true;
is_valid_type(cursor_entered) -> true;
is_valid_type(cursor_left) -> true;
is_valid_type(cursor_moved) -> true;
is_valid_type(destroyed) -> true;
is_valid_type(dropped_file) -> true;
is_valid_type(focused) -> true;
is_valid_type(hovered_file) -> true;
is_valid_type(hovered_file_cancelled) -> true;
is_valid_type(keyboard_input) -> true;
is_valid_type(mouse_input) -> true;
is_valid_type(mouse_wheel) -> true;
is_valid_type(moved) -> true;
is_valid_type(received_character) -> true;
is_valid_type(resized) -> true;
is_valid_type(scale_factor_changed) -> true;
is_valid_type(theme_changed) -> true;
is_valid_type(touch) -> true;
is_valid_type(touchpad_pressure) -> true;
is_valid_type(_) -> false.

position({_, {{type, cursor_moved}, [{x,X}, {y, Y}]}})-> {ok, {X, Y}};
position(_) -> {error, event_does_not_have_position}.
