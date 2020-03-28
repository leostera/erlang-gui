%% @doc
%%
%% An editable text field.
%%
%% It can:
%%  * display text honoring \n for newlines
%%  * scroll context
%%  * be focused/unfocused
%%
-module(text_field).

-behavior(gen_server).

-export([ start_link/2
        , init/1
        , terminate/2
        , handle_cast/2
        , handle_call/3
        ]).

-export([ draw/0
        , dump/0
        , start/0
        , restart/0
        , set_text/1
        , on_edit/1
, print_event/1
        ]).

%%==============================================================================
%% Behavior callbacks
%%==============================================================================

start_link(Args, Opts) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

init(Args) ->
  State = initial_state(Args),
  chalk_pipeline:register(fun ?MODULE:draw/0),
  chalk_event_server:register(fun ?MODULE:print_event/1),
  {ok, State}.

terminate(_, _) ->
  ok.

handle_call({on_edit, Fn}, _From, State) -> do_on_edit(Fn, State);
handle_call({set_text, T}, _From, State) -> do_set_text(T, State);
handle_call(draw, _From, State) -> do_draw(State);
handle_call(dump, _From, State) -> {reply, State, State};
handle_call(_Msg, _From, State) -> {noreply, State}.

handle_cast({_,{{type, mouse_input}, [{state,pressed}|_]}}, State) ->
  {noreply, do_focus(State)};
handle_cast({Ts, {{type, cursor_moved}, [{x,X}, {y,Y}]}}=Ev, State) ->
  {noreply, do_update_cursor(State, {X,Y})};
handle_cast({{type, keyboard_input},
             [ {kind, organic}
             , {input, [_,{state,pressed}, {virtual_keycode, Key}]}
             ]}=Msg, State) ->
  {noreply, do_type(State, Key)};
handle_cast({{type,_ }, _}=Msg, State) ->
  {noreply, State};

handle_cast(_Msg, State) -> {noreply, State}.

%%==============================================================================
%% Api
%%==============================================================================

print_event(E={Ts,_}) ->
  io:format("callback: ~pms\n", [(erlang:system_time()-Ts)/1000000]),
  gen_server:cast(?MODULE, E).

start() ->
  {ok, Pid} = text_field:start_link([],[]),
  text_field:set_text(lively:source_for_module(text_field)),
  {ok, Pid}.

restart() ->
  gen_server:stop(text_field),
  start().

draw() -> gen_server:call(?MODULE, draw).

dump() -> gen_server:call(?MODULE, dump).

set_text(Text) -> gen_server:call(?MODULE, {set_text, Text}).

on_edit(Fn) -> gen_server:call(?MODULE, {on_edit, Fn}).

%%==============================================================================
%% Internal
%%==============================================================================

initial_state(_) ->
  #{ pos => {200.0, 200.0, 1.0}
   , font_height => 30
   , dim => {1000, 1000}
   , mouse_pos => {0.0, 0.0}
   , status => unfocused
   , text => text:of_binary(<<"">>)
   , time => erlang:system_time()
   , callbacks => []
   }.

do_on_edit(Fn, State=#{ callbacks := Cb }) ->
  {reply, ok, State#{ callbacks => [Fn | Cb] }}.

do_type(State=#{ text := Text, callbacks := Cbs }, Key) ->
  case Key of
    'Back' -> text:delete(Text);
    'Space' -> text:write(Text, <<" ">>);
    'Return' -> text:write(Text, <<"\n">>);
    'Tab' -> text:write(Text, <<"\t">>);
    _ -> KeyBin = atom_to_binary(Key, utf8),
         text:write(Text, KeyBin)
  end,
  NewText = text:as_binary(Text),
  io:format("announcing to ~p\n", [Cbs]),
  [ (catch Cb(NewText)) || Cb <- Cbs ],
  State.

do_focus(State=#{ pos := {X, Y, _}
                , dim := {W, H}
                , mouse_pos := {MX, MY}
                , text := Text
                }) ->
  OnX = (X-W < MX) and (MX < X+W),
  OnY = (Y-W < MY) and (MY < Y+H),
  RelX = MX - X,
  RelY = MY - Y,
  case OnX and OnY of
    true ->
      io:format("hit: ~p\n",[{MX, MY}]),
      case {round(RelX/30), round(RelY/30)} of
        {Col, Line} when (Line > 0) and (Col > 0) ->
          text:move_cursor(Text, {Line, Col});
        _ -> ok
      end,

      State#{ status => focused };
    _ -> do_unfocus(State)
  end.

do_unfocus(S) -> S#{ status => unfocused }.

do_update_cursor(State=#{ pos := {X, Y, _}, text := Text }, Pos={MX, MY}) ->
  RelX = MX - X,
  RelY = MY - Y,

  case {round(RelX/30), round(RelY/30)} of
    {Col, Line} when (Line > 0) and (Col > 0) ->
      text:move_cursor(Text, {Line, Col});
    _ -> ok
  end,

  State#{ mouse_pos => Pos }.

do_set_text(Text, State=#{ font_height := H }) ->
  Buff = text:of_binary(Text),
  State2 = State#{ text => Buff
                 , dim  => {1000, length(text:lines(Buff)) * H + H }
                 },
  {reply, ok, State2}.

do_draw(#{ pos := Pos
         , dim := Dim
         , time := _
         , text := Text
         , status := Status
         , font_height := FontHeight
         }=State) ->
  Now = erlang:system_time(),
  { reply
  , {ok, Pos, field(FontHeight, Dim, Text, Status)}
  , State#{ time => Now }}.

field(FontHeight, Dim={W, H}, Text, Status) ->
  LineCount = length(text:lines(Text)),

  TextElement = text_element:draw(#{ dim => Dim
                                   , text => Text
                                   , font_height => FontHeight
                                   }),

  % Draw text

  Canvas = sk_canvas:new(W, H),
  sk_canvas:clip_rect(Canvas, W, H),

  DebugTextPaint = sk_paint:new(),
  sk_paint:set_style(DebugTextPaint, sk_paint:style_stroke_and_fill()),
  sk_paint:set_stroke_width(DebugTextPaint, 1.0),
  sk_paint:set_color(DebugTextPaint, sk_color:rgba(233, 80, 80, 255)),
  DebugFont = sk_font:default(),
  DebugTextBlob = sk_text_blob:from_binary(atom_to_list(Status), DebugFont),
  sk_canvas:draw_text_blob( Canvas
                          , DebugTextBlob
                          , W - 100
                          , 0
                          , DebugTextPaint),

  Paint = sk_paint:new(),
  sk_paint:set_style(Paint, sk_paint:style_stroke()),
  sk_paint:set_stroke_width(Paint, 3.0),
  sk_paint:set_color(Paint, sk_color:rgba(30, 30, 30, 255)),
  Rect = sk_rect:make_xywh(0, 0, W, H*(LineCount+1)),
  sk_canvas:draw_rect(Canvas, Rect, Paint),

  sk_canvas:draw_picture(Canvas, TextElement),

  {Line, Col} = text:cursor(Text),
  CursorRect = sk_rect:make_xywh((Col-1)*30, (Line-1)*30, 2, FontHeight),
  sk_canvas:draw_rect(Canvas, CursorRect, Paint),

  sk_picture:from_canvas(Canvas).
