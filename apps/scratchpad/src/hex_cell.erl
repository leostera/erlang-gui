-module(hex_cell).

-behavior(gen_server).

-export([ start_link/2
        , init/1
        , terminate/2
        , handle_cast/2
        , handle_call/3
        ]).

-export([ draw/1
        , on_hover/2
        ]).

%%==============================================================================
%% Behavior callbacks
%%==============================================================================

start_link(Args, Opts) -> gen_server:start_link(?MODULE, Args, Opts).

init(Args) ->
  State = initial_state(Args),
  Self = self(),
  {ok, NodeRef} = chalk:add_node(fun () -> hex_cell:draw(Self) end),
  chalk_event_server:register(NodeRef, fun (E) -> hex_cell:on_hover(Self, E) end), 
  {ok, State}.

terminate(_, _) -> ok.

handle_call(draw, _From, State) -> do_draw(State);
handle_call(_Msg, _From, State) -> {noreply, State}.

handle_cast(inc, State) -> do_inc(State);
handle_cast(_Msg, State) -> {noreply, State}.

%%==============================================================================
%% Api
%%==============================================================================

draw(Pid) -> gen_server:call(Pid, draw).

on_hover(Pid, _) -> gen_server:cast(Pid, inc).

%%==============================================================================
%% Internal
%%==============================================================================

initial_state(S=#{ size := Size, pos := {X, Y, _}}) ->
  S#{ dim => {Size*2, Size*2}
    , hover_time => 0
    , tile => chalk:new_frame({X, Y, 1.0}, {Size*2, Size*2}, tile(Size, 0))
    }.

do_inc(State=#{ hover_time := HT }) ->
  {noreply, State#{ hover_time => HT+10 }}.

do_draw(#{ pos := {X, Y, _}
         , dim := Dim
         , size := Size
         , tile := T
         , hover_time := HT
         }=State) ->
  {reply, T, State}.

tile(Size, 0) -> hex_lib:flat_hex_tile(Size);
tile(Size, T) -> hex_lib:flat_hex_tile(Size, paint(T)).

paint(T) ->
  Paint = sk_paint:new(),
  sk_paint:set_style(Paint, sk_paint:style_stroke()),
  sk_paint:set_stroke_width(Paint, 0.2),
  sk_paint:set_color(Paint, color(T)),
  Paint.

color(T) -> sk_color:rgba( 200+round(math:fmod( T, 255) * rand:uniform())
                         , 200+round(math:fmod( T, 255) * rand:uniform())
                         , 200+round(math:fmod( T, 255) * rand:uniform())
                         , 255
                         ).
