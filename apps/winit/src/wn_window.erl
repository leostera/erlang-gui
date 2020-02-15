-module(wn_window).

-export([ new/0
        ]).

-export_type([ t/0
             ]).

-record(wn_window, {}).

-opaque t() :: #wn_window{}.

new() -> #wn_window{}.
