-module(mqtt_simulator_data_simulators_config).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

%% API
-export([start_link/2,
         update_config/2]).

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

-spec update_config(term(), [mqtt_simulator_client_config:data()]) -> ok.
update_config(Id, Config) ->
    gen_server:cast(?VIA_GPROC(Id), {update_config, Config}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([SupId]) ->
    {ok, #state{data_simulator_sup_id = SupId}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({update_config, Config}, State=#state{data_simulator_sup_id = SupId}) ->
    ?LOG_INFO(#{what => update_config, supervisor_id => SupId}),
    lists:foreach(fun (DataPoint) ->
                          mqtt_simulator_data_simulators_sup:update_config(SupId, DataPoint)
                  end, Config),
    {noreply, State};

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
