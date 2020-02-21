-module(chalk_port).

-behavior(gen_server).

-define(CHALK_PORT, code:priv_dir(chalk) ++ "/native/chalk_port").

-export([ start_link/1
        , init/1
        , handle_info/2
        , handle_cast/2
        ]).

-export([ render/1
        ]).

start_link(_Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, _Args, []).

init(_Args) ->
  process_flag(trap_exit, true),
  Port = open_port({spawn, ?CHALK_PORT}, [binary]),
  {ok, #{ port => Port }}.

handle_info({_Port, {data, Ev}}, State) ->
  {noreply, State};
handle_info({'EXIT', _Port, _Reason}, _State) ->
  io:format("port_closed"),
  {noreply, #{ port => none }}.

handle_cast(quit, #{ port := Port }=State) ->
  Ref = make_ref(),
  port_command(Port, term_to_binary({Ref, quit})),
  {noreply, State};
handle_cast({Cmd, Data}, #{ port := Port }=State) ->
  Ref = make_ref(),
  port_command(Port, term_to_binary({Ref, Cmd, Data})),
  {noreply, State}.


render(Frame) -> gen_server:cast(?MODULE, {render, Frame}).
