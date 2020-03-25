%% @doc
%%
%% A text object.
%%
-module(text).

-behavior(gen_server).

-export([ start_link/2
        , init/1
        , terminate/2
        , handle_cast/2
        , handle_call/3
        ]).

-export([ start/0
        , cursor/1
        , delete/1
        , dump/1
        , lines/1
        , move_cursor/2
        , of_binary/1
        , as_binary/1
        , set_text/2
        , stop/1
        , write/2
        ]).

%%==============================================================================
%% Behavior callbacks
%%==============================================================================

start_link(Args, Opts) -> gen_server:start_link(?MODULE, Args, Opts).

init(Args) -> {ok, initial_state(Args)}.

terminate(_, _) -> ok.

handle_call(as_binary, _From, State) -> do_as_binary(State);
handle_call(cursor, _From, State) -> do_cursor(State);
handle_call(lines, _From, State) -> do_lines(State);
handle_call(delete, _From, State) -> do_delete(State);
handle_call({write, T}, _From, State) -> do_write(T, State);
handle_call({set_text, T}, _From, State) -> do_set_text(T, State);
handle_call({move_cursor, Pos}, _From, State) -> do_move_cursor(Pos, State);
handle_call(dump, _From, State) -> {reply, State, State};
handle_call(_Msg, _From, State) -> {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

%%==============================================================================
%% Api
%%==============================================================================

of_binary(Bin) when is_binary(Bin) ->
  {ok, Pid} = start(),
  set_text(Pid, Bin),
  Pid.

as_binary(Pid) when is_pid(Pid) ->
  gen_server:call(Pid, as_binary).

start() -> text:start_link([],[]).

stop(Pid) -> gen_server:stop(Pid).

dump(Pid) when is_pid(Pid) -> gen_server:call(Pid, dump).

set_text(Pid, Text) when is_pid(Pid) and is_binary(Text) ->
  gen_server:call(Pid, {set_text, Text}).

move_cursor(Pid, P={Line, Col}) when is_pid(Pid)
                                 and is_integer(Line) and (Line > 0)
                                 and is_integer(Col)  and (Col  > 0) ->
  gen_server:call(Pid, {move_cursor, P}).

write(Pid, Text) when is_pid(Pid) and is_binary(Text) ->
  gen_server:call(Pid, {write, Text}).

delete(Pid) when is_pid(Pid) -> gen_server:call(Pid, delete).

lines(Pid) when is_pid(Pid) -> gen_server:call(Pid, lines).

cursor(Pid) when is_pid(Pid) -> gen_server:call(Pid, cursor).


%%==============================================================================
%% Internal
%%==============================================================================

initial_state(_) ->
  #{ buffer => text_buffer:new(<<"">>)
   , cursor => {1, 1}
   , char_on_cursor => none
   }.

do_as_binary(State=#{ buffer := Buff }) ->
  {reply, text_buffer:text(Buff), State}.

do_cursor(State=#{ cursor := C }) -> {reply, C, State}.

do_lines(State=#{ buffer := Buff }) ->
  {reply, text_buffer:lines(Buff), State}.

do_delete(State=#{ cursor := Cursor={L, C}, buffer := Buff }) ->
  OldLineCount = text_buffer:line_count(Buff),
  {ok, NewBuff} = text_buffer:delete(Buff, Cursor),
  LineCount = text_buffer:line_count(NewBuff),
  NewCursor = case OldLineCount - LineCount of
                0 -> {L, max(C - 1, 1)};
                _ ->
                  NewLine = max(L - 1, 1),
                  {ok, LastNewLine} = text_buffer:line(NewBuff, NewLine),
                  {NewLine, text_buffer:column_count(LastNewLine)}
              end,
  {reply, ok, State#{ cursor => NewCursor
                    , char_on_cursor => text_buffer:char_at(NewBuff, NewCursor)
                    , buffer => NewBuff }}.



do_write(T, State=#{ cursor := Cursor={L, C}, buffer := Buff }) ->
  OldLineCount = text_buffer:line_count(Buff),
  {ok, NewBuff} = text_buffer:write(Buff, Cursor, T),
  LineCount = text_buffer:line_count(NewBuff),
  NewCursor = case OldLineCount - LineCount of
                0 -> {L, C + string:length(T) + 1};
                _ ->
                  NewLine = L + (LineCount-OldLineCount),
                  {ok, LastNewLine} = text_buffer:line(NewBuff, NewLine),
                  {NewLine, text_buffer:column_count(LastNewLine)}
              end,
  {reply, ok, State#{ cursor => NewCursor
                    , char_on_cursor => text_buffer:char_at(NewBuff, NewCursor)
                    , buffer => NewBuff }}.


do_set_text(T, State) ->
  Buff = text_buffer:new(T),
  Cursor = {1, 1},
  {reply, ok, State#{ buffer => Buff
                    , cursor => Cursor
                    , char_on_cursor => text_buffer:char_at(Buff, Cursor)
                    }}.

do_move_cursor({L, C}, State=#{ buffer := Buff }) ->
  case text_buffer:line(Buff, L) of
    {ok, Line} ->
      MaxC = text_buffer:column_count(Line),
      FinalC = max(min(C, MaxC), 1),

      NewCursor = {L, FinalC},
      Char = text_buffer:char_at(Buff, NewCursor),

      NewState = State#{ cursor => NewCursor
                       , char_on_cursor => Char
                       },

      {reply, {ok, NewCursor, Char}, NewState};
    {error, Reason} -> {reply, {error, Reason}, State}
  end.





