-module(mqtt_simulator_data_simulator_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(A_CLIENT_ID, <<"a client id">>).
-define(AN_ID, <<"an id">>).
-define(A_VALUE, <<"1">>).
-define(ANOTHER_VALUE, <<"2">>).
-define(A_CONFIG_WITH_MULTIPLE_VALUES, #{interval => 100,
                                         topic => <<"a topic">>,
                                         values => [?A_VALUE, ?ANOTHER_VALUE]}).
-define(A_CONFIG, #{interval => 100,
                    topic => <<"a topic">>,
                    values => [?A_VALUE]}).
-define(TIMEOUT, 5000).

all() ->
    [
     can_cycle_values,
     can_send_single_value
    ].

init_per_testcase(_Name, Config) ->
    {ok, Applications} = application:ensure_all_started(gproc),
    meck:new(mqtt_simulator_client),
    meck:expect(mqtt_simulator_client, publish, fun (_, _, _) -> ok end),
    [{applications, Applications} | Config].

end_per_testcase(_Name, Config) ->
    ok = lists:foreach(fun application:stop/1, ?config(applications, Config)),
    meck:unload(),
    Config.

can_cycle_values() ->
    [{doc, "Given multiple values, when reaching interval, then cycles to next value."}].
can_cycle_values(_Config) ->
    mqtt_simulator_data_simulator:start_link(?A_CLIENT_ID, ?AN_ID, ?A_CONFIG_WITH_MULTIPLE_VALUES),

    meck:wait(mqtt_simulator_client, publish, ['_', '_', ?A_VALUE], ?TIMEOUT),

    meck:wait(mqtt_simulator_client, publish, ['_', '_', ?ANOTHER_VALUE], ?TIMEOUT).

can_send_single_value() ->
    [{doc, "Given a single configured value, when reaching interval, then sends value."}].
can_send_single_value(_Config) ->
    mqtt_simulator_data_simulator:start_link(?A_CLIENT_ID, ?AN_ID, ?A_CONFIG),

    meck:wait(mqtt_simulator_client, publish, ['_', '_', ?A_VALUE], ?TIMEOUT).

%%%===================================================================
%%% Internal functions
%%%===================================================================
