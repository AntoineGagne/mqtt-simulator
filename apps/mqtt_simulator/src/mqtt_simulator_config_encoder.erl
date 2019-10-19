-module(mqtt_simulator_config_encoder).

%% API
-export([encode/1]).

-define(UUID, uuid:uuid_to_string(uuid:get_v4(), binary_standard)).

-type config() :: mqtt_simulator_client_config:config().

%%%===================================================================
%%% API
%%%===================================================================

-spec encode([config()] | config()) -> {ok, map() | [map()]} | {error, term()}.
encode(Configs) when is_list(Configs) ->
    lists:foldl(fun try_encode_config/2, {ok, []}, Configs);
encode(Config) ->
    encode_config(Config).

%%%===================================================================
%%% Internal functions
%%%===================================================================

try_encode_config(_, Error={error, _}) ->
    Error;
try_encode_config(Config, {ok, Configs}) ->
    case encode_config(Config) of
        {ok, Encoded} -> {ok, [Encoded | Configs]};
        Error -> Error
    end.

encode_config(Config) ->
    case do_encode_connection_info(Config) of
        Error={error, _} -> Error;
        Encoded -> {ok, Encoded}
    end.

do_encode_connection_info(Config) ->
    Parsed = mqtt_simulator_client_config:fold(fun encode_connection_info/3, #{}, Config),
    do_validate_keys(connection_info_mandatory_keys(), Parsed).

encode_connection_info(id, V, Acc) ->
    Acc#{<<"id">> => V};
encode_connection_info(host, V, Acc) ->
    Acc#{<<"host">> => V};
encode_connection_info(port, V, Acc) ->
    Acc#{<<"port">> => V};
encode_connection_info(username, V, Acc) ->
    Acc#{<<"username">> => V};
encode_connection_info(password, V, Acc) ->
    Acc#{<<"password">> => V};
encode_connection_info(reconnect_timeout, V, Acc) ->
    Acc#{<<"reconnectTimeout">> => V};
encode_connection_info(data, V, Acc) ->
    case encode_data(V) of
        Error={error, _} -> Error;
        Data -> Acc#{<<"data">> => Data}
    end;
encode_connection_info(Field, _, _) ->
    {error, {invalid_field, Field}}.

encode_data(Data) when is_list(Data) ->
    lists:foldl(fun do_encode_data/2, [], Data);
encode_data(Data) ->
    {error, {invalid_format, {not_list, Data}}}.

do_encode_data(Data, Acc) when is_map(Data) ->
    Encoded = maps:fold(fun encode_data_field/3, #{}, Data),
    case do_validate_keys(data_mandatory_keys(), Encoded) of
        Error={error, _} -> Error;
        _ ->
            WithId = Encoded#{id => ?UUID},
            [WithId | Acc]
    end;
do_encode_data(Data, _) ->
    {error, {invalid_format, {not_map, Data}}}.

encode_data_field(_, _, Error={error, _}) ->
    Error;
encode_data_field(id, V, Acc) ->
    Acc#{<<"id">> => V};
encode_data_field(values, V, Acc) when is_list(V) ->
    Acc#{<<"values">> => V};
encode_data_field(interval, V, Acc) when is_integer(V) ->
    Acc#{<<"interval">> => V};
encode_data_field(topic, V, Acc) when is_binary(V) ->
    Acc#{<<"topic">> => V};
encode_data_field(Field, _, _) ->
    {error, {invalid_field, Field}}.

do_validate_keys(_, Error={error, _}) ->
    Error;
do_validate_keys(Keys, Object) ->
    validate_mandatory_keys(Keys, Object).

validate_mandatory_keys(Keys, Object) ->
    case maps:with(Keys, Object) of
        WithKeys when map_size(WithKeys) >= length(Keys) ->
            Object;
        _ ->
            Missingkeys = lists:filter(fun (Key) -> not maps:is_key(Key, Object) end, Keys),
            {error, {missing_keys, Missingkeys}}
    end.

data_mandatory_keys() ->
    [<<"values">>, <<"interval">>, <<"topic">>].

connection_info_mandatory_keys() ->
    [<<"host">>, <<"port">>, <<"reconnectTimeout">>, <<"data">>].
