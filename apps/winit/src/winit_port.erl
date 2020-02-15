-module(winit_port).

-behavior(gen_server).

-define(WINIT_DRIVER, code:priv_dir(winit) ++ "/native/winit-port").

-export([
         p/0,
         s/1, q/1,
         start_link/1, init/1, handle_info/2,
         handle_cast/2
        ]).

p() ->
  open_port({spawn, ?WINIT_DRIVER}, [binary]).

s(P) ->
  port_command(P, term_to_binary({text, <<"message">>, {1,2,3}, [true]})).

q(P) ->
  port_command(P, term_to_binary(quit)).


start_link(_Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, _Args, []).

init(_Args) ->
  process_flag(trap_exit, true),
  Port = open_port({spawn, ?WINIT_DRIVER}, [binary]),
  {ok, #{ port => Port }}.

handle_info({Port, {data, Msg}}, State) ->
  {noreply, State}.

handle_cast({call, Msg}, #{ port := Port }=State) ->
  port_command(Port, Msg),
  {noreply, State}.
