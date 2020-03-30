%%%-------------------------------------------------------------------
%% @doc scratchpad Application
%% @end
%%%-------------------------------------------------------------------

-module(scratchpad_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  scratchpad_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
  ok.
