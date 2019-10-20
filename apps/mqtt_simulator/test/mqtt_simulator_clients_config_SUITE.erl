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
     restart_crashed_clients,
     return_error_on_config_update_failure,
     can_successfully_update_config,
     can_successfully_get_config,
     return_error_when_fetching_non_existent_config,
     can_successfully_get_configs
    ].

init_per_testcase(_Name, Config) ->
    Pid = make_pid(),
    meck:new(mqtt_simulator_clients_sup),
    meck:expect(mqtt_simulator_clients_sup, update_client, fun (_) -> ok end),
    meck:expect(mqtt_simulator_clients_sup, stop_client, fun (_) -> ok end),
    meck:expect(mqtt_simulator_clients_sup, start_client, fun (_) -> {ok, Pid} end),
    [{pid, Pid} | Config].

end_per_testcase(_Name, Config) ->
    meck:unload(),
    Config.

stop_client_on_new_config_without_client() ->
    [{doc, "Given a connected client, when receiving a new configuration without the "
      "specified client, then stops the client."}].
stop_client_on_new_config_without_client(Config) ->
    Pid = ?config(pid, Config),
    unlink(Pid),
    {ok, _} = mqtt_simulator_clients_config:start_link(?SYNC_INTERVAL),
    mqtt_simulator_clients_config:update_configs([?A_CONFIG]),

    mqtt_simulator_clients_config:update_configs([?ANOTHER_CONFIG]),

    meck:wait(mqtt_simulator_clients_sup, stop_client, ['_'], ?TIMEOUT).

start_client_on_new_config() ->
    [{doc, "Given a new configuration with a client, when receiving the configuration, "
      "then starts the client."}].
start_client_on_new_config(_Config) ->
    {ok, _} = mqtt_simulator_clients_config:start_link(?SYNC_INTERVAL),

    mqtt_simulator_clients_config:update_configs([?A_CONFIG]),

    meck:wait(mqtt_simulator_clients_sup, start_client, ['_'], ?TIMEOUT).

restart_crashed_clients() ->
    [{doc, "Given crashed client, when synchronizing, then restarts crashed clients."}].
restart_crashed_clients(Config) ->
    Pid = ?config(pid, Config),
    unlink(Pid),
    {ok, _} = mqtt_simulator_clients_config:start_link(?SYNC_INTERVAL),
    mqtt_simulator_clients_config:update_configs([?A_CONFIG]),
    meck:wait(mqtt_simulator_clients_sup, start_client, ['_'], ?TIMEOUT),

    exit(Pid, kill),

    meck:wait(2, mqtt_simulator_clients_sup, start_client, ['_'], ?TIMEOUT).

return_error_on_config_update_failure() ->
    [{doc, "Given an configuration update error, when updating configuration, "
      "then returns error."}].
return_error_on_config_update_failure(_Config) ->
    meck:expect(mqtt_simulator_clients_sup, update_client, fun (_) -> {error, error} end),
    {ok, _} = mqtt_simulator_clients_config:start_link(?SYNC_INTERVAL),

    ?assertMatch({error, _}, mqtt_simulator_clients_config:update_config(?ANOTHER_CONFIG)).

can_successfully_update_config() ->
    [{doc, "Given a new configuration, when updating configuration, then returns success."}].
can_successfully_update_config(_Config) ->
    {ok, _} = mqtt_simulator_clients_config:start_link(?SYNC_INTERVAL),

    ?assertMatch(ok, mqtt_simulator_clients_config:update_config(?ANOTHER_CONFIG)).

can_successfully_get_config() ->
    [{doc, "Given an existing configuration, when fetching configuration, "
      "then returns configuration."}].
can_successfully_get_config(_Config) ->
    {ok, _} = mqtt_simulator_clients_config:start_link(?SYNC_INTERVAL),
    mqtt_simulator_clients_config:update_configs([?A_CONFIG]),

    ?assertMatch({ok, _}, mqtt_simulator_clients_config:get_config(?AN_ID)).

return_error_when_fetching_non_existent_config() ->
    [{doc, "Given a non-existent configuration, when fetching configuration, "
      "then returns an error."}].
return_error_when_fetching_non_existent_config(_Config) ->
    {ok, _} = mqtt_simulator_clients_config:start_link(?SYNC_INTERVAL),

    ?assertMatch({error, _}, mqtt_simulator_clients_config:get_config(?AN_ID)).

can_successfully_get_configs() ->
    [{doc, "Given existing configuration, when fetching configurations, "
      "then returns all the configuration."}].
can_successfully_get_configs(_Config) ->
    {ok, _} = mqtt_simulator_clients_config:start_link(?SYNC_INTERVAL),

    ?assertMatch([], mqtt_simulator_clients_config:get_configs()).

%%%===================================================================
%%% Internal functions
%%%===================================================================

make_pid() ->
    spawn_link(fun Loop() ->
                       timer:sleep(60000),
                       Loop()
               end).
