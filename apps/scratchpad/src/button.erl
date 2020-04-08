-module(button).

-behavior(gen_server).

-export([ start_link/2
        , init/1
        , terminate/2
        , handle_cast/2
        , handle_call/3
        ]).

-export([ draw/1
        , dump/1
        , start/0
        , start_n/1
        , handle_event/2
        ]).

%%==============================================================================
%% Behavior callbacks
%%==============================================================================

start_link(Args, Opts) ->
  gen_server:start_link(?MODULE, Args, Opts).

init(Args) ->
  State = initial_state(Args),
  Self = self(),
  {ok, NodeRef} = chalk:add_node(fun () -> button:draw(Self) end),
  chalk_event_server:register(NodeRef, fun (E) -> ?MODULE:handle_event(Self, E) end),
  {ok, State}.

terminate(_, _) -> ok.

handle_call(draw, _From, State) -> do_draw(State);
handle_call(dump, _From, State) -> {reply, State, State};
handle_call(_Msg, _From, State) -> {noreply, State}.

handle_cast({event, {Ts, _}}, State) ->
  io:format("~pms to cast\n\n",[(erlang:system_time()-Ts)/1000000]),
  {noreply, State};

handle_cast(_Msg, State) -> {noreply, State}.

%%==============================================================================
%% Api
%%==============================================================================

start_n(0) -> ok;
start_n(N) -> start(), start_n(N-1).

start() -> button:start_link([],[]).

draw(Self) -> gen_server:call(Self, draw).

dump(Self) -> gen_server:call(Self, dump).

handle_event(Self, Event={Ts,_}) ->
  io:format("~pms to handle\n",[(erlang:system_time()-Ts)/1000000]),
  gen_server:cast(Self, {event, Event}).

%%==============================================================================
%% Internal
%%==============================================================================

initial_state(_) ->
  #{ box => box()
   , pos => {rand:uniform(1000)*1.0, rand:uniform(1000)*1.0, 1.0}
   , dim => {rand:uniform(100)*1.0, rand:uniform(100)*1.0}
   }.

box() ->
  C = sk_canvas:new(100, 00),
  R = sk_rect:make_xywh(0, 0, 100, 100),
  P = line_paint(),
  sk_canvas:draw_rect(C, R, P),
  sk_picture:from_canvas(C).

do_draw(#{ pos := Pos
         , dim := Dim
         }=State) ->
  {reply, chalk:new_frame(Pos, Dim, box()), State}.

line_paint() ->
  Paint = sk_paint:new(),
  sk_paint:set_style(Paint, sk_paint:style_stroke()),
  sk_paint:set_stroke_width(Paint, 1.0),
  sk_paint:set_color(Paint, sk_color:rgba(20,20,20,255)),
  Paint.
