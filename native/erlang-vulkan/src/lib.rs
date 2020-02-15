#[macro_use]
extern crate rustler;

mod atoms {
    rustler_atoms! {
        atom ok;
        //atom error;
        //atom __true__ = "true";
        //atom __false__ = "false";
    }
}

rustler::rustler_export_nifs! {
    "Elixir.Vulkan.Native",
    [
    ],
    None
}
