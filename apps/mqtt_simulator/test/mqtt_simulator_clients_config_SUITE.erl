-module(mqtt_simulator_clients_config_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(SYNC_INTERVAL, 500).
-define(AN_ID, <<"an id">>).
-define(ANOTHER_ID, <<"another id">>).
-define(A_CONFIG, #{id => ?AN_ID}).
-define(ANOTHER_CONFIG, #{id => ?ANOTHER_ID}).
-define(TIMEOUT, 5000).

all() ->
    [
     stop_client_on_new_config_without_client,
     start_client_on_new_config,
     restart_crashed_clients
    ].

init_per_testcase(_Name, Config) ->
    Pid = make_pid(),
    meck:new(mqtt_simulator_clients_sup),
    meck:expect(mqtt_simulator_clients_sup, stop_client, fun (_) -> ok end),
    meck:expect(mqtt_simulator_clients_sup, start_client, fun (_) -> {ok, Pid} end),
    [{pid, Pid} | Config].

end_per_testcase(_Name, Config) ->
    Config.

stop_client_on_new_config_without_client() ->
    [{doc, "Given a connected client, when receiving a new configuration without the "
      "specified client, then stops the client."}].
stop_client_on_new_config_without_client(Config) ->
    Pid = ?config(pid, Config),
    unlink(Pid),
    {ok, _} = mqtt_simulator_clients_config:start_link(?SYNC_INTERVAL),
    mqtt_simulator_clients_config:update_config([?A_CONFIG]),

    mqtt_simulator_clients_config:update_config([?ANOTHER_CONFIG]),

    meck:wait(mqtt_simulator_clients_sup, stop_client, ['_'], ?TIMEOUT).

start_client_on_new_config() ->
    [{doc, "Given a new configuration with a client, when receiving the configuration, "
      "then starts the client."}].
start_client_on_new_config(_Config) ->
    {ok, _} = mqtt_simulator_clients_config:start_link(?SYNC_INTERVAL),

    mqtt_simulator_clients_config:update_config([?A_CONFIG]),

    meck:wait(mqtt_simulator_clients_sup, start_client, ['_'], ?TIMEOUT).

restart_crashed_clients() ->
    [{doc, "Given crashed client, when synchronizing, then restarts crashed clients."}].
restart_crashed_clients(Config) ->
    Pid = ?config(pid, Config),
    unlink(Pid),
    {ok, _} = mqtt_simulator_clients_config:start_link(?SYNC_INTERVAL),
    mqtt_simulator_clients_config:update_config([?A_CONFIG]),
    meck:wait(mqtt_simulator_clients_sup, start_client, ['_'], ?TIMEOUT),

    exit(Pid, kill),

    meck:wait(2, mqtt_simulator_clients_sup, start_client, ['_'], ?TIMEOUT).

%%%===================================================================
%%% Internal functions
%%%===================================================================

make_pid() ->
    spawn_link(fun Loop() ->
                       timer:sleep(60000),
                       Loop()
               end).
