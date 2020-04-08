-module(sk_text_blob).

-export([ from_binary/2
        , from_string/2
        ]).

from_string(Text, Font) when is_list(Text) ->
  from_binary(binary:list_to_bin(Text), Font).

from_binary(Text, Font) when is_binary(Text) ->
  skia_native:sk_text_blob__from_binary(binary:bin_to_list(Text), Font).
