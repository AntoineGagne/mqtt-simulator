-module(mqtt_simulator_config_encoder_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

all() ->
    [
     return_error_when_config_in_list_is_invalid,
     return_error_on_non_list_data,
     return_error_on_non_map_data
    ].

init_per_testcase(_Name, Config) ->
    C = mqtt_simulator_client_config:init(),
    ConfigWithNonMapData = mqtt_simulator_client_config:data([<<"">>], C),
    ConfigWithNonListData = mqtt_simulator_client_config:data(#{}, C),

    [{a_config_with_non_map_data, ConfigWithNonMapData},
     {a_config_with_non_list_data, ConfigWithNonListData} | Config].

end_per_testcase(_Name, Config) ->
    Config.

return_error_when_config_in_list_is_invalid() ->
    [{doc, "Given an invalid configuration, when encoding configurations, then returns an error."}].
return_error_when_config_in_list_is_invalid(Config) ->
    C = ?config(a_config_with_non_list_data, Config),
    Valid = mqtt_simulator_client_config:init(),
    Configs = [Valid, C, C],

    ?assertMatch({error, _}, mqtt_simulator_config_encoder:encode(Configs)).

return_error_on_non_list_data() ->
    [{doc, "Given data that are not a list, when parsing configuration, then returns error."}].
return_error_on_non_list_data(Config) ->
    C = ?config(a_config_with_non_list_data, Config),

    ?assertMatch({error, {invalid_format, {not_list, _}}},
                 mqtt_simulator_config_encoder:encode(C)).

return_error_on_non_map_data() ->
    [{doc, "Given data that are not a map, when parsing configuration, then returns error."}].
return_error_on_non_map_data(Config) ->
    C = ?config(a_config_with_non_map_data, Config),
    ?assertMatch({error, {invalid_format, {not_map, _}}},
                 mqtt_simulator_config_encoder:encode(C)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
