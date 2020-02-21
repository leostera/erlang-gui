-module(binbuf).

-export([
         new/1,
         get/2,
         set/3
        ]).

-compile(no_native).
-on_load(on_load/0).

on_load() ->
  SoName = code:priv_dir(?MODULE) ++ "/native/liberlbinbuf",
  erlang:load_nif(SoName, ok).

-define(NIF_ERROR, erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE})).

new(_Size) -> ?NIF_ERROR.
get(_Buff,_Offset) -> ?NIF_ERROR.
set(_Buff,_Offset,_Val) -> ?NIF_ERROR.
