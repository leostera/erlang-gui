-module(text_buffer).

-compile([export_all]).

new(Text) ->
  Table = make_table(),
  ok = fill_table(Table, Text),
  #{ table => Table
   , text => Text
   , lines => lines(Table) }.

text(#{ text := T }) -> T.

delete(#{ table := T, text := OldText }, {L, C}) ->
  CharCount = cursor_to_char_count(T, {L, C}),
  NewText = << (string:slice(OldText, 0, CharCount-1))/binary
             , (string:slice(OldText, CharCount))/binary
             >>,
  ok = fill_table(T, NewText),
  NewTextBuffer = #{ table => T
                   , text => NewText
                   , lines => lines(T) },
  {ok, NewTextBuffer}.

write(#{ table := T, text := OldText }, {L, C}, Text) ->
  CharCount = cursor_to_char_count(T, {L, C}),
  NewText = << (string:slice(OldText, 0, CharCount))/binary
             , Text/binary
             , (string:slice(OldText, CharCount))/binary
             >>,
  ok = fill_table(T, NewText),
  NewTextBuffer = #{ table => T
                   , text => NewText
                   , lines => lines(T) },
  {ok, NewTextBuffer}.

cursor_to_char_count(T, {L, C}) ->
  Elements = ets:tab2list(T),
  lists:foldl(fun ({{L1, _}, _}, Acc) when (L1 < L) -> Acc+1;
                  ({{L1, C1}, _}, Acc) when (L1 =:= L) and (C1 =< C) -> Acc+1;
                  (_, Acc)  -> Acc
              end, 0, Elements).

char_at(#{ table := T }, {L, C}) ->
  case ets:lookup(T, {L, C}) of
    [] -> none;
    [{{L,C},X}] -> X
  end.

line_count(S) -> length(lines(S)).
column_count(Line) -> string:length(Line).

line(S, N) when is_integer(N) and (N > 0) ->
  L = line_count(S),
  case N =< L of
    true -> {ok, lists:nth(N, lines(S))};
    _ -> {error, {reason, out_of_range, {min, 1}, {max, L}, {asked, N}}}
  end.


lines(#{ table := T }) -> lines(T);
lines(T) when is_reference(T) ->
  Lines = ets:foldl(fun
                      ({_, <<"\n">>}, [Curr|Acc]) -> [<<"">>, << Curr/binary, "\n">>|Acc];
                      ({_, C}, [Curr|Acc]) -> [<< Curr/binary, C/binary >>|Acc]
                    end
                    , [<<"">>]
                    , T),
  lists:reverse(Lines).

table(#{ table := T }) -> ets:tab2list(T).

make_table() ->
  ets:new(text_buffer, [ordered_set,{write_concurrency,true},{read_concurrency,true}]).

fill_table(Table, Text) ->
  ets:delete_all_objects(Table),
  Tokens = lists:droplast(re:split(Text,"",[{return,list}])),
  lists:foldl(
    fun ("\n", {Line, Col}) -> ets:insert(Table, {{Line, Col}, <<"\n">>}),
                               {Line+1, 1};
        (C, {Line, Col}) -> ets:insert(Table, {{Line, Col}, binary:list_to_bin(C)}),
                            {Line, Col+1}
    end, {1, 1}, Tokens),
  ok.

