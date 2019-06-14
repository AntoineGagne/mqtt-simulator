-module(mqtt_simulator_data_simulators_config_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(AN_ID, <<"an id">>).
-define(ANOTHER_ID, <<"another id">>).
-define(A_CONFIG, #{id => ?AN_ID}).
-define(ANOTHER_CONFIG, #{id => ?ANOTHER_ID}).
-define(SYNC_INTERVAL, 500).
-define(TIMEOUT, 5000).

all() ->
    [
     stop_data_simulator_on_new_config_without_data_simulator,
     start_data_simulator_on_new_config,
     restart_crashed_data_simulators
    ].

init_per_testcase(_Name, Config) ->
    Pid = make_pid(),
    {ok, Applications} = application:ensure_all_started(gproc),
    meck:new(mqtt_simulator_data_simulators_sup),
    meck:expect(mqtt_simulator_data_simulators_sup, stop_data_simulator, fun (_, _) -> ok end),
    meck:expect(mqtt_simulator_data_simulators_sup, start_data_simulator, fun (_, _) -> {ok, Pid} end),
    [{pid, Pid}, {applications, Applications} | Config].

end_per_testcase(_Name, Config) ->
    ok = lists:foreach(fun application:stop/1, ?config(applications, Config)),
    meck:unload(),
    Config.

stop_data_simulator_on_new_config_without_data_simulator() ->
    [{doc, "Given a connected data simulator, when receiving a new configuration without the "
      "specified data simulator, then stops the data simulator."}].
stop_data_simulator_on_new_config_without_data_simulator(Config) ->
    Pid = ?config(pid, Config),
    unlink(Pid),
    {ok, _} = mqtt_simulator_data_simulators_config:start_link(?AN_ID, ?AN_ID, ?SYNC_INTERVAL),
    mqtt_simulator_data_simulators_config:update_config(?AN_ID, [?A_CONFIG]),

    mqtt_simulator_data_simulators_config:update_config(?AN_ID, [?ANOTHER_CONFIG]),

    meck:wait(mqtt_simulator_data_simulators_sup, stop_data_simulator, ['_', '_'], ?TIMEOUT).

start_data_simulator_on_new_config() ->
    [{doc, "Given a new configuration with a data simulator, when receiving the configuration, "
      "then starts the data simulator."}].
start_data_simulator_on_new_config(_Config) ->
    {ok, _} = mqtt_simulator_data_simulators_config:start_link(?AN_ID, ?AN_ID, ?SYNC_INTERVAL),

    mqtt_simulator_data_simulators_config:update_config(?AN_ID, [?A_CONFIG]),

    meck:wait(mqtt_simulator_data_simulators_sup, start_data_simulator, ['_', '_'], ?TIMEOUT).

restart_crashed_data_simulators() ->
    [{doc, "Given crashed data simulator, when synchronizing, then restarts crashed "
      "data simulator."}].
restart_crashed_data_simulators(Config) ->
    Pid = ?config(pid, Config),
    unlink(Pid),
    {ok, _} = mqtt_simulator_data_simulators_config:start_link(?AN_ID, ?AN_ID, ?SYNC_INTERVAL),
    mqtt_simulator_data_simulators_config:update_config(?AN_ID, [?A_CONFIG]),
    meck:wait(mqtt_simulator_data_simulators_sup, start_data_simulator, ['_', '_'], ?TIMEOUT),

    exit(Pid, kill),

    meck:wait(2, mqtt_simulator_data_simulators_sup, start_data_simulator, ['_', '_'], ?TIMEOUT).

%%%===================================================================
%%% Internal functions
%%%===================================================================

make_pid() ->
    spawn_link(fun Loop() ->
                       timer:sleep(60000),
                       Loop()
               end).
