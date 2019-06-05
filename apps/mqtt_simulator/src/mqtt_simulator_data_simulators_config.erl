-module(mqtt_simulator_data_simulators_config).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(VIA_GPROC(Id), {via, gproc, {n, l, Id}}).
-define(SERVER, ?MODULE).

-record(state, {data_simulator_sup_id :: term()}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(term(), term()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Id, SupId) ->
    gen_server:start_link(?VIA_GPROC(Id), ?MODULE, [SupId], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([SupId]) ->
    {ok, #state{data_simulator_sup_id = SupId}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
