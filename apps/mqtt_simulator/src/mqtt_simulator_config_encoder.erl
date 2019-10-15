-module(mqtt_simulator_config_encoder).

%% API
-export([encode/1]).

-type config() :: mqtt_simulator_client_config:config().

%%%===================================================================
%%% API
%%%===================================================================

-spec encode([config()] | config()) -> {ok, binary()} | {error, term()}.
encode(Configs) ->
    jsone:try_encode(Configs).

%%%===================================================================
%%% Internal functions
%%%===================================================================
