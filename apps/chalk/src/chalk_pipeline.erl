-module(chalk_pipeline).

-behaviour(gen_server).

-export([ start_link/1
        , stop/0
        , init/1
        , terminate/2
        , handle_call/3
        , handle_cast/2
        ]).

-export([ next_frame/0
        , queue/1
        , in_queue/0
        , drop/0
        ]).

%%===============================================================================
%%
%% Behavior callbacks
%%
%%===============================================================================

start_link(_Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:stop(?MODULE).

init(_Args) ->
  {ok, #{ frames => queue:new() }}.

terminate(_, _) -> ok.

handle_call({next_frame}, _From, State) ->
  {Reply, NewState} = do_next_frame(State),
  {reply, Reply, NewState};

handle_call({drop_queue}, _From, _State) ->
  {ok, State} = init([]),
  {reply, ok, State};

handle_call({in_queue}, _From, #{ frames := Fs }=State) ->
  {reply, queue:len(Fs), State};

handle_call({queue, Frame}, _From, State) ->
  {reply, ok, do_queue(Frame, State)}.

handle_cast(_Msg, State) -> {noreply, State}.

%%===============================================================================
%% Api
%%===============================================================================

in_queue() -> gen_server:call(?MODULE, {in_queue}).

next_frame() -> gen_server:call(?MODULE, {next_frame}).

queue(Frame) -> gen_server:call(?MODULE, {queue, Frame}).

drop() -> gen_server:call(?MODULE, {drop_queue}).

%%===============================================================================
%% Internal
%%===============================================================================

do_next_frame(#{ frames := Fs }) ->
  case queue:out(Fs) of
    {empty, Fs0} -> {{error, no_more_frames}, #{ frames => Fs0}};
    {{value, F}, Fs1} -> {{ok, F}, #{ frames => Fs1 }}
  end.

do_queue(F, #{ frames := Fs }) -> #{ frames => queue:in(F, Fs) }.
