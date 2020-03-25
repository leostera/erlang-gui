-module(hex_cell).

-behavior(gen_server).

-export([ start_link/2
        , init/1
        , terminate/2
        , handle_cast/2
        , handle_call/3
        ]).

-export([ draw/1
        ]).

%%==============================================================================
%% Behavior callbacks
%%==============================================================================

start_link(Args, Opts) -> gen_server:start_link(?MODULE, Args, Opts).

init(Args) ->
  State = initial_state(Args),
  Self = self(),
  %chalk_event_server:register(Self),
  chalk_pipeline:register(fun () -> hex_cell:draw(Self) end),
  {ok, State}.

terminate(_, _) -> ok.

handle_call(draw, _From, State) -> do_draw(State);
handle_call(_Msg, _From, State) -> {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

%%==============================================================================
%% Api
%%==============================================================================

draw(Pid) -> gen_server:call(Pid, draw).

%%==============================================================================
%% Internal
%%==============================================================================

initial_state(S=#{ size := Size }) ->
  S#{ red_tile => hex_lib:flat_hex_tile(Size, red_paint())
    , green_tile => hex_lib:flat_hex_tile(Size, green_paint())
    }.

do_draw(#{ pos := Pos
         , tile := Tile
         , red_tile := RedTile
         , green_tile := GreenTile
         , time := T
         }=State) ->
  Now = erlang:system_time(),
  Delta = Now - T,
  {reply, {ok, Pos, Tile}, State#{ time => Now }}.

green_paint() ->
  Paint = sk_paint:new(),
  sk_paint:set_style(Paint, sk_paint:style_fill()),
  sk_paint:set_stroke_width(Paint, 10.0),
  sk_paint:set_color(Paint, sk_color:rgba(80,233,80,255)),
  Paint.

red_paint() ->
  Paint = sk_paint:new(),
  sk_paint:set_style(Paint, sk_paint:style_fill()),
  sk_paint:set_stroke_width(Paint, 10.0),
  sk_paint:set_color(Paint, sk_color:rgba(233,80,80,255)),
  Paint.
