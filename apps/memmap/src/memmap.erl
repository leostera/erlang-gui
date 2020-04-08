-module(memmap).

-compile({no_auto_import,[size/1]}).

-export([ open_read/2
        , open_write/2
        , zero/1
        , read/3
        , size/1
        , write/3
        , write_term/3
        ]).

size(#{ size := Size }) -> Size.

zero(Memmap) ->
  Size = size(Memmap),
  write(Memmap, << 0:Size >>, 0).

open_read(Path, Size) when is_binary(Path)
                       and is_integer(Size) and (Size > 0) ->
  #{ ref => memmap_native:open_read(Path, Size)
   , size => Size }.

open_write(Path, Size) when is_binary(Path)
                        and is_integer(Size) and (Size > 0) ->
  #{ ref => memmap_native:open_write(Path, Size)
   , size => Size }.

read(#{ ref := Memmap, size := MaxSize }, Size, Offset)
  when is_reference(Memmap)
   and is_integer(Size) and is_integer(Offset)
   and (Size > 0) and (Offset >= 0)
   and ((Size+Offset) =< MaxSize) ->
  {ok, memmap_native:read(Memmap, Size, Offset)};
read(#{ size := MaxSize }, Size, Offset) ->
  {error, { read_out_of_bounds
          , {max, MaxSize}
          , {off_by, abs((Size+Offset) - MaxSize)}}}.

write_term(Memmap, Term, Offset) ->
  Bin = term_to_binary(Term),
  Size = << (byte_size(Bin)):32 >>,
  {ok, FinalOffset} = write(Memmap, <<0:8, Size/binary, Bin/binary>>, Offset),
  {ok, _} = write(Memmap, <<1:8>>, Offset),
  {ok, FinalOffset}.

write(#{ ref := Memmap, size := MaxSize }, Data, Offset)
  when is_reference(Memmap)
   and is_binary(Data) and is_integer(Offset)
   and (Offset >= 0)
   and ((byte_size(Data)+Offset) =< MaxSize) ->
  memmap_native:write(Memmap, Data, Offset),
  {ok, byte_size(Data)+Offset};
write(#{ size := MaxSize }, Data, Offset) ->
  {error, { write_out_of_bounds
          , {max, MaxSize}
          , {off_by, abs((byte_size(Data)+Offset) - MaxSize)}}}.
