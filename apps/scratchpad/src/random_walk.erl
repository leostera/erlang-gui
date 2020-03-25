-module(random_walk).

-behavior(gen_server).

-export([ start_link/2
        , init/1
        , terminate/2
        , handle_cast/2
        , handle_call/3
        ]).

-export([ move/2, draw/0, dump/0, restart/0, start/0 ]).

start() ->
  random_walk:start_link([],[]),
  chalk_pipeline:flush().

restart() ->
  gen_server:stop(random_walk),
  chalk_pipeline:clear(),
  start().

%%==============================================================================
%% Behavior callbacks
%%==============================================================================

start_link(Args, Opts) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

init(Args) ->
  State = initial_state(Args),
  {ok, do_init(State)}.

terminate(_, _) -> ok.

handle_call({move, Pos}, _From, State) -> do_move(State, Pos);
handle_call(draw, _From, State) -> do_draw(State);
handle_call(dump, _From, State) -> {reply, State, State};
handle_call(_Msg, _From, State) -> {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

%%==============================================================================
%% Api
%%==============================================================================

dump() -> gen_server:call(?MODULE, dump).

draw() -> gen_server:call(?MODULE, draw).

move(X, Y) -> gen_server:call(?MODULE, {move, {X, Y, 1.0}}).

%%==============================================================================
%% Internal
%%==============================================================================

initial_state(_) ->
  #{ pos => { 1500.0, 1500.0, 1.0 }
   , size => 100
   , last_time => erlang:system_time()
   , steps => []
   }.

do_init(State=#{ size := S }) ->
  chalk_pipeline:register(fun () -> {ok, {0.0,0.0,0.0}, hex_lib:bg()} end),
  chalk_pipeline:register(fun () -> random_walk:draw() end),
  Tile = hex_lib:flat_hex_tile(S),
  State#{ tile => Tile }.


do_move(S, Pos) -> {reply, ok, S#{ pos => Pos }}.

do_draw(#{ tile := Tile
         , pos := Pos
         , last_time := LastT
         , steps := LastSteps
         }=State) ->
  Now = erlang:system_time(),
  Steps = step_count_since(LastT, Now),
  NewPos = lists:foldl( fun (_, LastPos) -> animate(LastPos, random_dir()) end
                      , Pos
                      , Steps),
  NewState = State#{ pos => NewPos
                   , last_time => case Steps of
                                    [] -> LastT;
                                    _  -> Now
                                  end
                   },
  {reply, {ok, NewPos, Tile}, NewState}.


move_speed() -> 50.0.

animate({X, Y, Z}, up)    -> {X - move_speed(), Y, Z};
animate({X, Y, Z}, left)  -> {X, Y - move_speed(), Z};
animate({X, Y, Z}, down)  -> {X + move_speed(), Y, Z};
animate({X, Y, Z}, right) -> {X, Y + move_speed(), Z}.

random_dir() -> random_dir(rand:uniform(4)).
random_dir(1) -> up;
random_dir(2) -> left;
random_dir(3) -> down;
random_dir(4) -> right.

step_count_since(T, Now) ->
  Delta = (Now - T) / (10*1000*1000),
  case round(Delta) div 1 of
    0 -> [];
    N -> lists:seq(0, N)
  end.
