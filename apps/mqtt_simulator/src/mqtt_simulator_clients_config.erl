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
-define(TABLE_NAME, mqtt_simulator_clients_config_ids_by_pids).

-record(state, {sync_interval :: pos_integer(),
                sync_timer :: reference(),
                pids_by_ids = #{} :: #{binary() := pid()},
                configs_by_ids = #{} :: configs_by_ids()}).
-record(diff, {to_start = #{} :: configs_by_ids(),
               to_stop = #{} :: configs_by_ids()}).

-type configs_by_ids() :: #{binary() := mqtt_simulator_client_config:config()}.

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
    process_flag(trap_exit, true),
    SyncTimer = erlang:start_timer(SyncInterval, self(), synchronize),
    _ = ets:new(?TABLE_NAME, [set, named_table, private]),
    {ok, #state{sync_timer = SyncTimer,
                sync_interval = SyncInterval}}.

handle_call({update_config, Configs}, _From, State) ->
    NewConfigs = to_config_map(Configs),
    Diff = diff(NewConfigs, State#state.configs_by_ids),
    UpdatedState = apply_diff(Diff, State),
    {reply, ok, UpdatedState#state{configs_by_ids = NewConfigs}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, TimerRef, synchronize}, State=#state{sync_timer = TimerRef,
                                                           configs_by_ids = ConfigsByIds,
                                                           pids_by_ids = PidsByIds,
                                                           sync_interval = SyncInterval}) ->
    Diff = diff(ConfigsByIds, PidsByIds),
    UpdatedState = apply_diff(Diff, State),
    SyncTimer = erlang:start_timer(SyncInterval, self(), synchronize),
    {noreply, UpdatedState#state{sync_timer = SyncTimer}};

handle_info({timeout, _TimerRef, synchronize}, State) ->
    {noreply, State};

handle_info({'EXIT', Pid, Reason}, State=#state{pids_by_ids = PidsByIds}) ->
    ?LOG_INFO(#{what => process_exited, pid => Pid, reason => Reason}),
    Predicate = fun ({_, {Pid2, _}}) -> Pid =:= Pid2 end,
    case lists:search(Predicate, maps:to_list(PidsByIds)) of
        {value, {Id, _}} ->
            {noreply, State#state{pids_by_ids = maps:remove(Id, PidsByIds)}};
        false ->
            {noreply, State}
    end;

handle_info(Message, State) ->
    ?LOG_WARNING(#{what => unknown_message_received, message => Message}),
    {noreply, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

diff(Configs, PidsByIds) ->
    Ids = maps:keys(Configs),
    Started = maps:keys(PidsByIds),
    ToStop = maps:without(Ids, PidsByIds),
    ToStart = maps:without(Started, Configs),
    #diff{to_stop = ToStop, to_start = ToStart}.

apply_diff(#diff{to_stop = ToStop, to_start = ToStart}, State) ->
    PidsByIds = maps:fold(fun stop/3, State#state.pids_by_ids, ToStop),
    State#state{pids_by_ids = maps:fold(fun start/3, PidsByIds, ToStart)}.

to_config_map(Configs) ->
    F = fun (Config, ConfigsByIds) ->
                Id = mqtt_simulator_client_config:id(Config),
                ConfigsByIds#{Id => Config}
        end,
    lists:foldl(F, #{}, Configs).

stop(Id, _, PidsByIds) ->
    case maps:take(Id, PidsByIds) of
        {{Pid, Ref}, PidsByIds2} ->
            true = erlang:demonitor(Ref, [flush]),
            ok = mqtt_simulator_clients_sup:stop_client(Pid),
            PidsByIds2;
        error ->
            PidsByIds
    end.

start(Id, Config, PidsByIds) ->
    case mqtt_simulator_clients_sup:start_client(Config) of
        {ok, Pid} ->
            Ref = erlang:monitor(process, Pid),
            PidsByIds#{Id => {Pid, Ref}};
        _ ->
            PidsByIds
    end.
