-module(chalk_event_handler).

-behavior(gen_event).

-export([ init/1
        , terminate/2
        , handle_call/2
        , handle_event/2
        , handle_info/2
        ]).

init(_) -> {ok, #{}}.

terminate(_, _) -> ok.

handle_event(Event={event, _Kind, _Status, _Which}, State) ->
  io:format("~p", [Event]),
  {ok, State}.

handle_call(_, State) ->
  {ok, State}.

handle_info(_, State) ->
  {ok, State}.

