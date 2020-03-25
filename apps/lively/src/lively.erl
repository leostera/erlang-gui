-module(lively).

-compile([export_all]).

reload(Src) when is_list(Src) ->
  reload(binary:list_to_bin(Src));
reload(Src) when is_binary(Src) ->
  {ok, M, BC, _} = lively:compile(Src),
  {module, M} = lively:load(M, BC),
  ok.

%% @doc This is the starting function.
prepare(F, M) ->
  {ok, M, BC, _} = lively:compile(F),
  Src = lively:source(BC),
  {module, M} = lively:load(M, BC),
  #{ module => M
   , source => Src
   , bytecode => BC
   }.

source_for_module(M) ->
  {M, Bc, _} = code:get_object_code(M),
  source(Bc).

load(Name, BC) -> code:load_binary(Name, Name, BC).

check(F) -> compile:file(F, check_opts()).

compile(F) when is_list(F) -> compile:file(F, compile_opts());
compile(Src) when is_binary(Src) ->
  Parts = string:split(Src, <<"\n\n">>, all),
  Trees = lists:filtermap(fun (P) ->
                        Str = binary:bin_to_list(P),
                        {ok, F, _} = erl_scan:string(Str),
                        case F of
                          [] -> false;
                          _ -> {true, F}
                        end
                    end, Parts),
  Forms = lists:map(fun (F) ->
                        {ok, T} = erl_parse:parse_form(F),
                        T
                    end, Trees),
  compile:forms(Forms, compile_opts()).


compile_opts() ->
  [ verbose
  , binary
  , return
  , debug_info
  , warnings_as_errors
  , nowarn_export_all
  , deterministic
  ].

check_opts() -> [ strong_validation | compile_opts() ].

source(Bytecode) ->
  {ok,{_,[{abstract_code,{_,AC}}]}} = beam_lib:chunks(Bytecode,[abstract_code]),
  Src = erl_prettypr:format(erl_syntax:form_list(AC)),
  binary:list_to_bin(Src).

