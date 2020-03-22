-module(chalk_port).

-behavior(gen_server).

-define(CHALK_PORT, code:priv_dir(chalk) ++ "/native/chalk_port").

-export([ start_link/2
        , stop/0
        , init/1
        , terminate/2
        , handle_info/2
        , handle_cast/2
        , handle_call/3
        ]).

-export([ render/1
        ]).

%%==============================================================================
%% Behavior callbacks
%%==============================================================================

start_link(Args, Opts) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, Opts).

stop() ->
  gen_server:stop(?MODULE).

init(_Args) ->
  process_flag(trap_exit, true),
  Port = open_port({spawn, ?CHALK_PORT}, [binary]),
  {ok, #{ port => Port }}.

terminate(_, #{ port := Port }) ->
  port_close(Port),
  ok.

handle_info({_Port, {data, Ev}}, State) ->
  handle_event(Ev),
  {noreply, State};
handle_info({'EXIT', _Port, _Reason}, _State) ->
  {noreply, #{ port => none }}.

handle_cast(quit, #{ port := Port }=State) ->
  Ref = make_ref(),
  port_command(Port, term_to_binary({Ref, quit})),
  {noreply, State};
handle_cast({Kind, Data}, #{ port := Port }=State) ->
  ok = do_command(Port, Kind, Data),
  {noreply, State}.

handle_call(_, _, State) ->
  {noreply, State}.

%%==============================================================================
%% Api
%%==============================================================================

render(Frame) when is_binary(Frame)-> gen_server:cast(?MODULE, {render, Frame}).

%%==============================================================================
%% Internal
%%==============================================================================

handle_event(Ev) when is_binary(Ev) ->
  case (catch binary_to_term(Ev)) of
    {'EXIT', _} -> ok;
    Term -> handle_event(Term)
  end;

handle_event({{ref, none}, {kind, relay}, {data, Event}}) ->
  chalk_event_server:send(Event);
handle_event(_) -> ok.


do_command(Port, Kind, Data) ->
  Ref = make_ref(),
  Cmd = {{ref,Ref}, {kind,Kind}, {data,Data}},
  Msg = term_to_binary(Cmd),
  port_command(Port, Msg),
  ok.
