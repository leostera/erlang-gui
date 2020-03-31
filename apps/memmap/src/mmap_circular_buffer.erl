-module(mmap_circular_buffer).

-export([
         open_write/2,
         write_term/2
        ]).

open_write(Path, Size) ->
  Mmap = memmap:open_write(Path, Size),
  #{ map => Mmap, offset => 0 }.

write_term(#{ map := Memmap, offset := Offset }, Term) ->
  Bin = term_to_binary(Term),
  Size = << (byte_size(Bin)):32 >>,
  Data = <<0:8, Size/binary, Bin/binary>>,
  SafeOffset = next_offset(Data, Offset, memmap:size(Memmap)),
  {ok, FinalOffset} = memmap:write(Memmap, Data, SafeOffset),
  {ok, _} = memmap:write(Memmap, <<1:8>>, SafeOffset),
  {ok, #{ map => Memmap, offset => FinalOffset }}.

next_offset(Data, Offset, MaxSize) when (byte_size(Data)+Offset) > MaxSize -> 0;
next_offset(_, Offset, _) -> Offset.

