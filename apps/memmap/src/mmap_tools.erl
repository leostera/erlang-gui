-module(mmap_tools).

-compile([export_all]).

pair(Size) ->
  Path = <<"/home/ostera/repos/github.com/ostera/erlang-gui/test-0.mmap">>,
  W = memmap:open_write(Path, Size),
  R = memmap:open_read(Path, Size),
  {W, R}.

data() -> data(<<>>, 1024*1024*10).
data(Acc, 0) -> Acc;
data(Acc, N) -> data(<< 0,1,2,3,4,5,6,7,8,9,0, Acc/binary>>, N-1).

test(Data) ->
  Size = byte_size(Data),
  {W, R} = pair(Size),
  Offset = 0,
  {ok, Offset} = memmap:write(W, Data, Offset),
  {ok, Data} = memmap:read(R, Size, Offset),
  Data.
