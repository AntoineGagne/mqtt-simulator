-module(mqtt_simulator_data_simulator_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

all() ->
    [
     can_cycle_values,
     can_send_single_value
    ].

init_per_testcase(_Name, Config) ->
    Config.

end_per_testcase(_Name, Config) ->
    Config.

can_cycle_values() ->
    [{doc, "Given multiple values, when reaching interval, then cycles to next value."}].
can_cycle_values(_Config) ->
    ok.

can_send_single_value() ->
    [{doc, "Given a single configured value, when reaching interval, then sends value."}].
can_send_single_value(_Config) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
