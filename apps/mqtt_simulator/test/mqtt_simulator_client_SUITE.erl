-module(mqtt_simulator_client_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(AN_ID, <<"an id">>).
-define(A_CONFIG_ID, <<"a config id">>).
-define(A_CONFIG, #{username => <<"a username">>,
                    password => <<"a password">>,
                    id => ?AN_ID,
                    port => 1883,
                    host => <<"127.0.0.1">>,
                    data => [],
                    reconnect_timeout => 100}).
-define(AN_INCOMPLETE_CONFIG, #{id => ?AN_ID,
                                port => 1883,
                                host => <<"127.0.0.1">>,
                                data => [],
                                reconnect_timeout => 100}).
-define(A_TOPIC, <<"a topic">>).
-define(A_PAYLOAD, <<"a payload">>).
-define(TIMEOUT, 5000).

all() ->
    [
     reconnect_on_connect_errors,
     connect_with_complete_configuration,
     connect_with_incomplete_configuration,
     reconnect_on_client_process_errors,
     can_publish_data_on_connected_client
    ].

init_per_testcase(_Name, Config) ->
    Pid = make_pid(),
    {ok, Applications} = application:ensure_all_started(gproc),
    meck:new(emqttc),

    meck:expect(emqttc, start_link, fun (_) ->
                                            self() ! {mqttc, Pid, connected},
                                            link(Pid),
                                            {ok, Pid}
                                    end),
    meck:expect(emqttc, publish, fun (_, _, _) -> ok end),
    [{pid, Pid}, {applications, Applications} | Config].

end_per_testcase(_Name, Config) ->
    ok = lists:foreach(fun application:stop/1, ?config(applications, Config)),
    meck:unload(),
    Config.

reconnect_on_connect_errors() ->
    [{doc, "Given a connection error, when connecting, then reconnects."}].
reconnect_on_connect_errors(_Config) ->
    meck:expect(emqttc, start_link, fun (_) -> {error, error} end),

    {ok, _} = mqtt_simulator_client:start_link(?AN_ID, ?A_CONFIG_ID, ?A_CONFIG),

    meck:wait(2, emqttc, start_link, ['_'], ?TIMEOUT).

connect_with_complete_configuration() ->
    [{doc, "Given a complete configuration, when connecting, then connects."}].
connect_with_complete_configuration(_Config) ->
    {ok, _} = mqtt_simulator_client:start_link(?AN_ID, ?A_CONFIG_ID, ?A_CONFIG),

    meck:wait(emqttc, start_link, ['_'], ?TIMEOUT).

connect_with_incomplete_configuration() ->
    [{doc, "Given an incomplete configuration, when connecting, then connects."}].
connect_with_incomplete_configuration(_Config) ->
    {ok, _} = mqtt_simulator_client:start_link(?AN_ID, ?A_CONFIG_ID, ?AN_INCOMPLETE_CONFIG),

    meck:wait(emqttc, start_link, ['_'], ?TIMEOUT).

reconnect_on_client_process_errors() ->
    [{doc, "Given a connected client, when client crashes, then reconnects."}].
reconnect_on_client_process_errors(Config) ->
    Pid = ?config(pid, Config),
    unlink(Pid),
    {ok, _} = mqtt_simulator_client:start_link(?AN_ID, ?A_CONFIG_ID, ?A_CONFIG),
    meck:wait(emqttc, start_link, ['_'], ?TIMEOUT),

    exit(Pid, crash),

    meck:wait(2, emqttc, start_link, ['_'], ?TIMEOUT).

can_publish_data_on_connected_client() ->
    [{doc, "Given a connected client, when publishing, then publishes data."}].
can_publish_data_on_connected_client(_Config) ->
    {ok, _} = mqtt_simulator_client:start_link(?AN_ID, ?A_CONFIG_ID, ?A_CONFIG),
    meck:wait(emqttc, start_link, ['_'], ?TIMEOUT),

    mqtt_simulator_client:publish(?AN_ID, ?A_TOPIC, ?A_PAYLOAD),

    meck:wait(emqttc, publish, ['_', '_', '_'], ?TIMEOUT).

%%%===================================================================
%%% Internal functions
%%%===================================================================

make_pid() ->
    spawn_link(fun Loop() ->
                       timer:sleep(60000),
                       Loop()
               end).
