%%%-------------------------------------------------------------------
%% @doc scratchpad top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(scratchpad_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
  {ok, { supervision_flags(), child_specs() } }.

%%====================================================================
%% Internal functions
%%====================================================================

supervision_flags() -> #{ strategy  => one_for_all
                        , intensity => 5
                        , period    => 5
                        }.

child_specs() -> [].
%  [ #{ id => bg
%     , start => {bg, start, []}
%     , restart => permanent
%     , shutdown => brutal_kill
%     , type => worker
%     , modules => [bg]
%     }
%  , #{ id => fps
%     , start => {fps, start, []}
%     , restart => permanent
%     , shutdown => brutal_kill
%     , type => worker
%     , modules => [fps]
%     }
%  , #{ id => mouse_coords
%     , start => {mouse_coords, start, []}
%     , restart => permanent
%     , shutdown => brutal_kill
%     , type => worker
%     , modules => [mouse_coords]
%     }
%  ].
