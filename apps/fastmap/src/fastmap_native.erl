-module(fastmap_native).

-export([
         new/0,
         insert/3,
         as_list/1
        ]).

-compile(no_native).
-on_load(on_load/0).

on_load() ->
  SoName = code:priv_dir(fastmap) ++ "/native/liberlfastmap",
  erlang:load_nif(SoName, ok).

-define(NIF_ERROR, erlang:nif_error({nif_not_loaded,module,?MODULE,line,?LINE})).

new() -> ?NIF_ERROR.
insert(_Map,_Key,_Val) -> ?NIF_ERROR.
as_list(_Map) -> ?NIF_ERROR.
