-module(mqtt_simulator_clients_config_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

all() ->
    [
     stop_client_on_new_config_without_client,
     start_client_on_new_config,
     restart_crashed_clients
    ].

init_per_testcase(_Name, Config) ->
    Config.

end_per_testcase(_Name, Config) ->
    Config.

stop_client_on_new_config_without_client() ->
    [{doc, "Given a connected client, when receiving a new configuration without the "
      "specified client, then stops the client."}].
stop_client_on_new_config_without_client(_Config) ->
    ok.

start_client_on_new_config() ->
    [{doc, "Given a new configuration with a client, when receiving the configuration, "
      "then starts the client."}].
start_client_on_new_config(_Config) ->
    ok.

restart_crashed_clients() ->
    [{doc, "Given crashed client, when synchronizing, then restarts crashed clients."}].
restart_crashed_clients(_Config) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
