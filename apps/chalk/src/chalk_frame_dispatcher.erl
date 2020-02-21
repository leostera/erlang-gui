-module(chalk_frame_dispatcher).

-behaviour(gen_server).

-export([ start_link/1
        , init/1
        , handle_cast/2
        ]).

-export([ start/0
        , stop/0
        , dispatch/0
        ]).

start_link(_Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, _Args, []).

init(_Args) ->
  {ok, Timer} = do_start(),
  {ok, #{ timer => Timer }}.

handle_cast(stop, #{ timer := T }=State) ->
  {ok, cancel} = do_stop(T),
  {noreply, State};
handle_cast(start, _State) ->
  {ok, Timer} = do_start(),
  {noreply, #{ timer => Timer}}.


start() -> gen_server:cast(?MODULE, start).

stop() -> gen_server:cast(?MODULE, stop).


do_start() -> timer:apply_interval(16, ?MODULE, dispatch, []).

do_stop(T) -> timer:cancel(T).

dispatch() ->
  case chalk_pipeline:next_frame() of
       {ok, F} -> chalk_port:render(F);
       _ ->
    ok
  end.
