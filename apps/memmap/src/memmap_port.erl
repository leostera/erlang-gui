-module(memmap_port).

-define(SIZE_TAG_SIZE, 5).

-export([ open_port/3, port_close/1, loop/1 ]).

%%==============================================================================
%% Api
%%==============================================================================

open_port(Owner, Path, FileSize) ->
  InitialState = initialize({Owner, FileSize, Path}),
  Pid = spawn_link(fun () ->
                       Res = (catch loop(InitialState)),
                       io:format("ERROR: ~p\n\n\n", [Res])
                   end),
  {ok, Pid}.

port_close(Pid) -> exit(Pid, kill).

%%==============================================================================
%% Internal
%%==============================================================================

initialize({Owner, Size, Path}) ->
  R = memmap:open_read(Path, Size),
  #{ path => Path
   , reader => R
   , file_size => Size
   , last_read_time => erlang:system_time()
   , last_term => none
   , offset => 0
   , owner => Owner
   }.

loop(#{ reader := R
      , offset := Offset
      , owner := Owner
      }=State) ->
  Now = erlang:system_time(),
  SizeTag = read_size_tag(R, Offset),
  case SizeTag of
    locked -> memmap_port:loop(State#{ last_read_time => Now });
    {free, 0} -> memmap_port:loop(State#{ last_read_time => Now });
    {free, Size} ->
      {ok, Term} = read_term(R, Size, Offset+?SIZE_TAG_SIZE),
      forward_data(Term, Owner, Now),
      NewState = State#{ last_read_time => Now
                       , last_term => Term
                       , offset => Offset+?SIZE_TAG_SIZE+Size
                       },
      memmap_port:loop(NewState)
  end.

forward_data(Term, Owner, Now) -> Owner ! {self(), {data, Term}}.

%%==============================================================================
%% Protocol
%%==============================================================================

read_size_tag(R, Offset) ->
  {ok, Data} = memmap:read(R, ?SIZE_TAG_SIZE, Offset),
  decode_size_tag(Data).

decode_size_tag(<<0:8, _:32>>) -> locked;
decode_size_tag(<<_:8, Size:32>>) -> {free, Size}.

read_term(R, Size, Offset) ->
  {ok, Data} = memmap:read(R, Size, Offset),
  {ok, Data}.
