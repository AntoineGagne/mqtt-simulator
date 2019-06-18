-module(mqtt_simulator_config_parser).

%% API
-export([parse/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec parse(binary()) -> {ok, mqtt_simulator_client_config:config()}.
parse(_Config) ->
    {ok, mqtt_simulator_client_config:init()}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
