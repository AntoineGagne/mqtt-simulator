-module(mqtt_simulator_data_simulator_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

all() ->
    [
     reconnect_on_connect_errors,
     connect_with_incomplete_configuration,
     reconnect_on_client_process_errors,
     can_publish_data_on_connected_client
    ].

init_per_testcase(_Name, Config) ->
    Config.

end_per_testcase(_Name, Config) ->
    Config.

reconnect_on_connect_errors() ->
    [{doc, "Given a connection error, when connecting, then reconnects."}].
reconnect_on_connect_errors(_Config) ->
    ok.

connect_with_incomplete_configuration() ->
    [{doc, "Given an incomplete configuration, when connecting, then connects."}].
connect_with_incomplete_configuration(_Config) ->
    ok.

reconnect_on_client_process_errors() ->
    [{doc, "Given a connected client, when client crashes, then reconnects."}].
reconnect_on_client_process_errors(_Config) ->
    ok.

can_publish_data_on_connected_client() ->
    [{doc, "Given a connected client, when publishing, then publishes data."}].
can_publish_data_on_connected_client(_Config) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
