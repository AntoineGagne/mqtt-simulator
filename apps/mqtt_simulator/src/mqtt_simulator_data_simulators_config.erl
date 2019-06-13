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
         handle_info/2]).

-define(VIA_GPROC(Id), {via, gproc, {n, l, Id}}).
-define(SERVER, ?MODULE).
-define(DEFAULT_SYNCHRONIZATION_INTERVAL, 60000).

-record(state, {data_simulator_sup_id :: term(),
                sync_interval = ?DEFAULT_SYNCHRONIZATION_INTERVAL :: pos_integer(),
                sync_timer :: reference(),
                pids_by_ids = #{} :: #{binary() := pid()},
                configs_by_ids = #{} :: configs_by_ids()}).
-record(diff, {to_start = #{} :: configs_by_ids(),
               to_stop = #{} :: configs_by_ids()}).

-type configs_by_ids() :: #{binary() := mqtt_simulator_client_config:data()}.

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
    process_flag(trap_exit, true),
    SyncTimer = erlang:start_timer(?DEFAULT_SYNCHRONIZATION_INTERVAL, self(), synchronize),
    {ok, #state{data_simulator_sup_id = SupId,
                sync_timer = SyncTimer}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({update_config, Configs}, State) ->
    NewConfigs = to_config_map(Configs),
    Diff = diff(NewConfigs, State#state.configs_by_ids),
    UpdatedState = apply_diff(Diff, State),
    {noreply, UpdatedState#state{configs_by_ids = NewConfigs}};

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

handle_info({'DOWN', _Ref, process, Pid, Reason}, State=#state{pids_by_ids = PidsByIds}) ->
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
    PidsByIds = maps:fold(stop(State), State#state.pids_by_ids, ToStop),
    State#state{pids_by_ids = maps:fold(start(State), PidsByIds, ToStart)}.

to_config_map(Configs) ->
    F = fun (Config, ConfigsByIds) ->
                Id = mqtt_simulator_client_config:id(Config),
                ConfigsByIds#{Id => Config}
        end,
    lists:foldl(F, #{}, Configs).

stop(#state{data_simulator_sup_id = SupId}) ->
    fun (Id, _, PidsByIds) ->
            case maps:take(Id, PidsByIds) of
                {{Pid, Ref}, PidsByIds2} ->
                    true = erlang:demonitor(Ref, [flush]),
                    ok = mqtt_simulator_data_simulators_sup:stop_data_simulator(SupId, Pid),
                    PidsByIds2;
                error ->
                    PidsByIds
            end
    end.

start(#state{data_simulator_sup_id = SupId}) ->
    fun (Id, DataPoint, PidsByIds) ->
            case mqtt_simulator_data_simulators_sup:start_data_simulator(SupId, DataPoint) of
                {ok, Pid} ->
                    Ref = erlang:monitor(process, Pid),
                    PidsByIds#{Id => {Pid, Ref}};
                _ ->
                    PidsByIds
            end
    end.
