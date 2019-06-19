-module(mqtt_simulator_config_parser_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(A_CONFIG_WITH_INVALID_FORMAT, []).
-define(A_CONFIG_WITH_NON_LIST_DATA,
        #{<<"host">> => <<"">>,
          <<"port">> => 1883,
          <<"reconnectTimeout">> => 5000,
          <<"data">> => #{}}).
-define(A_CONFIG_WITH_NON_MAP_DATA,
        #{<<"host">> => <<"">>,
          <<"port">> => 1883,
          <<"reconnectTimeout">> => 5000,
          <<"data">> => [<<"">>]}).


all() ->
    [
     return_error_on_invalid_config_format,
     return_error_on_empty_config,
     return_error_on_non_list_data,
     return_error_on_non_map_data
    ].

init_per_testcase(_Name, Config) ->
    Config.

end_per_testcase(_Name, Config) ->
    Config.

return_error_on_invalid_config_format() ->
    [{doc, "Given an invalid configuration format, when parsing configuration, "
      "then returns error."}].
return_error_on_invalid_config_format(_Config) ->
    ?assertMatch({error, _}, mqtt_simulator_config_parser:parse(?A_CONFIG_WITH_INVALID_FORMAT)).

return_error_on_empty_config() ->
    [{doc, "Given an empty configuration, when parsing configuration, then returns error."}].
return_error_on_empty_config(_Config) ->
    ?assertMatch({error, _}, mqtt_simulator_config_parser:parse(#{})).

return_error_on_non_list_data() ->
    [{doc, "Given data that are not a list, when parsing configuration, then returns error."}].
return_error_on_non_list_data(_Config) ->
    ?assertMatch({error, {invalid_format, {not_list, _}}},
                 mqtt_simulator_config_parser:parse(?A_CONFIG_WITH_NON_LIST_DATA)).

return_error_on_non_map_data() ->
    [{doc, "Given data that are not a map, when parsing configuration, then returns error."}].
return_error_on_non_map_data(_Config) ->
    ?assertMatch({error, {invalid_format, {not_map, _}}},
                 mqtt_simulator_config_parser:parse(?A_CONFIG_WITH_NON_MAP_DATA)).

%%%===================================================================
%%% Internal functions
%%%===================================================================
