-module(memmap_native).

-export([
         open_read/2,
         open_write/2,
         read/3,
         write/3
        ]).

-compile(no_native).
-on_load(on_load/0).

on_load() ->
  SoName = code:priv_dir(memmap) ++ "/native/liberlmemmap",
  erlang:load_nif(SoName, ok).

-define(NIF_ERROR, erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE})).

open_read(_Path, _Size) -> ?NIF_ERROR.
open_write(_Path, _Size) -> ?NIF_ERROR.
read(_Memmap, _Size, _Offset) -> ?NIF_ERROR.
write(_Memmap, _Data, _Offset) -> ?NIF_ERROR.
