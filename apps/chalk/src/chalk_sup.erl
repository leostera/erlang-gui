%%%-------------------------------------------------------------------
%% @doc chalk top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(chalk_sup).

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

child_specs() ->
  [ #{ id => chalk_node_tree
     , start => {chalk_node_tree, start_link, [none, []]}
     , restart => permanent
     , type => worker
     , modules => [chalk_node_tree]
     }
  , #{ id => chalk_event_server
     , start => {chalk_event_server, start_link, [none, []]}
     , restart => permanent
     , type => worker
     , modules => [chalk_event_server]
     }
  , #{ id => chalk_render_pipeline
     , start => {chalk_render_pipeline, start_link, [none, []]}
     , restart => permanent
     , type => worker
     , modules => [chalk_render_pipeline]
     }
  , #{ id => chalk_port
     , start => {chalk_port, start_link, [none, []]}
     , restart => permanent
     , type => worker
     , modules => [chalk_port]
     }
  ].
