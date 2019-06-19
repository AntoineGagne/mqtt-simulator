-module(mqtt_simulator_config_parser_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(A_CONFIG_WITH_INVALID_FORMAT, []).

all() ->
    [
     return_error_on_invalid_config_format,
     return_error_on_empty_config
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

%%%===================================================================
%%% Internal functions
%%%===================================================================
