-module(mqtt_simulator_client).

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(VIA_GPROC(Id), {via, gproc, {n, l, Id}}).
-define(SERVER, ?MODULE).

-record(state, {config_id :: term(),
                config :: mqtt_simulator_client_config:config()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Id, ConfigId, Config) ->
    gen_server:start_link(?VIA_GPROC(Id), ?MODULE, [ConfigId, Config], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ConfigId, Config]) ->
    {ok, #state{config_id = ConfigId, config = Config}}.

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
