-module(mqtt_simulator_clients_config).

-include_lib("kernel/include/logger.hrl").

-behaviour(gen_server).

%% API
-export([start_link/1,
         update_config/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).

-define(SERVER, ?MODULE).

-record(state, {sync_interval :: pos_integer(),
                sync_timer :: reference(),
                configs_by_ids = #{} :: #{binary() := mqtt_simulator_client_config:config()}}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(pos_integer()) -> {ok, pid()} | ignore | {error, term()}.
start_link(SyncInterval) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [SyncInterval], []).

-spec update_config([mqtt_simulator_client_config:config()]) -> ok.
update_config(Config) ->
    gen_server:call(?SERVER, {update_config, Config}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([SyncInterval]) ->
    SyncTimer = erlang:start_timer(SyncInterval, self(), synchronize),
    {ok, #state{sync_timer = SyncTimer,
                sync_interval = SyncInterval}}.

handle_call({update_config, Configs}, _From, State) ->
    ok = lists:foreach(fun mqtt_simulator_clients_sup:start_client/1, Configs),
    {reply, ok, State#state{configs_by_ids = to_config_map(Configs)}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, TimerRef, synchronize}, State=#state{sync_timer = TimerRef,
                                                           sync_interval = SyncInterval}) ->
    SyncTimer = erlang:start_timer(SyncInterval, self(), synchronize),
    {noreply, State#state{sync_timer = SyncTimer}};

handle_info({timeout, _TimerRef, synchronize}, State) ->
    {noreply, State};

handle_info(Message, State) ->
    ?LOG_WARNING(#{what => unknown_message_received, message => Message}),
    {noreply, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

to_config_map(Configs) ->
    F = fun (Config, ConfigsByIds) ->
                Id = mqtt_simulator_client_config:id(Config),
                ConfigsByIds#{Id => Config}
        end,
    lists:foldl(F, #{}, Configs).
