-module(button).

-behavior(gen_server).

-export([ start_link/2
        , init/1
        , terminate/2
        , handle_cast/2
        , handle_call/3
        ]).

-export([ draw/0
        , start/0
        , restart/0
        ]).

%%==============================================================================
%% Behavior callbacks
%%==============================================================================

start_link(Args, Opts) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

init(Args) ->
  State = initial_state(Args),
  chalk_pipeline:register(fun () -> {ok, {0.0,0.0,0.0}, hex_lib:bg()} end),
  chalk_pipeline:register(fun () -> button:draw() end),
  chalk_event_server:register(self()),
  {ok, State}.

terminate(_, _) -> ok.

handle_call(draw, _From, State) -> do_draw(State);
handle_call(_Msg, _From, State) -> {noreply, State}.

handle_cast({{type, mouse_input}, [{state,pressed}|_]}, State) ->
  {noreply, do_click(State)};
handle_cast({{type, mouse_input}, [{state,released}|_]}, State) ->
  {noreply, do_unclick(State)};
handle_cast({{type, cursor_moved}, [{x,X}, {y,Y}]}, State) ->
  {noreply, do_update_cursor(State, {X,Y})};
handle_cast(_Msg, State) ->
  {noreply, State}.

%%==============================================================================
%% Api
%%==============================================================================

start() ->
  button:start_link([],[]),
  chalk_pipeline:flush().

restart() ->
  gen_server:stop(button),
  chalk_pipeline:clear(),
  start().

draw() -> gen_server:call(?MODULE, draw).

%%==============================================================================
%% Internal
%%==============================================================================

initial_state(_) ->
  #{ hex => hex_lib:flat_hex_tile(100)
   , pos => {1000.0, 1000.0, 1.0}
   , dim => {100, 100}
   , mouse_pos => {0.0, 0.0}
   , status => unclicked
   , time => erlang:system_time()
   , on_click => fun (#{ mouse_pos := P}) -> io:format("click at ~p\n",[P]) end
   }.

do_click(State=#{ pos := {X, Y, _}
                , dim := {W, H}
                , mouse_pos := {MX, MY}
                , on_click := Fn
                }) ->
  OnX = (X-W < MX) and (MX < X+W),
  OnY = (Y-W < MY) and (MY < Y+H),
  case OnX and OnY of
    true -> State2 = State#{ status => clicked },
            Fn(State2),
            State2;
    _ -> do_unclick(State)
  end.

do_unclick(S) -> S#{ status => unclicked }.

do_update_cursor(State, Pos) -> State#{ mouse_pos => Pos }.

do_draw(#{ pos := Pos
         , hex := Hex
         , time := _
         }=State) ->
  Now = erlang:system_time(),
  {reply, {ok, Pos, Hex}, State#{ time => Now }}.
