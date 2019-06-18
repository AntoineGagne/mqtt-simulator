-module(mqtt_simulator_config_parser).

-include_lib("kernel/include/logger.hrl").

%% API
-export([parse/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec parse(term()) -> {ok, mqtt_simulator_client_config:config()}.
parse(Raw) ->
    Parsed = maps:fold(fun parse_connection_info/3, #{}, Raw),
    Validated = do_validate_keys(connection_info_mandatory_keys(), Parsed),
    to_config(Validated).

%%%===================================================================
%%% Internal functions
%%%===================================================================

to_config(Error={error, _}) ->
    Error;
to_config(Parsed) ->
    Config = maps:fold(fun to_config/3, mqtt_simulator_client_config:init(), Parsed),
    {ok, Config}.

to_config(host, V, Config) ->
    mqtt_simulator_client_config:host(V, Config);
to_config(port, V, Config) ->
    mqtt_simulator_client_config:port(V, Config);
to_config(username, V, Config) ->
    mqtt_simulator_client_config:username(V, Config);
to_config(password, V, Config) ->
    mqtt_simulator_client_config:password(V, Config);
to_config(reconnect_timeout, V, Config) ->
    mqtt_simulator_client_config:reconnect_timeout(V, Config);
to_config(data, V, Config) ->
    mqtt_simulator_client_config:data(V, Config).

parse_connection_info(_, _, Error={error, _}) ->
    Error;
parse_connection_info(<<"host">>, V, Acc) when is_binary(V) ->
    Acc#{host => V};
parse_connection_info(<<"port">>, V, Acc) when is_integer(V) ->
    Acc#{port => V};
parse_connection_info(<<"username">>, V, Acc) when is_binary(V) ->
    Acc#{username => V};
parse_connection_info(<<"password">>, V, Acc) when is_binary(V) ->
    Acc#{password => V};
parse_connection_info(<<"reconnectTimeout">>, V, Acc) when is_integer(V) ->
    Acc#{reconnect_timeout => V};
parse_connection_info(<<"data">>, V, Acc) ->
    case parse_data(V) of
        Error={error, _} -> Error;
        Data -> Acc#{data => Data}
    end;
parse_connection_info(Field, _, _) ->
    {error, {invalid_field, Field}}.

parse_data(Data) when is_list(Data) ->
    lists:foldl(fun do_parse_data/2, [], Data);
parse_data(Data) ->
    {error, {invalid_format, {not_list, Data}}}.

do_parse_data(Data, Acc) when is_map(Data) ->
    Parsed = maps:fold(fun parse_data_field/3, #{}, Data),
    case do_validate_keys(data_mandatory_keys(), Parsed) of
        Error={error, _} -> Error;
        _ ->
            WithId = Parsed#{id => uuid:uuid_to_string(uuid:get_v4(), binary_standard)},
            [WithId | Acc]
    end;
do_parse_data(Data, _) ->
    {error, {invalid_format, {not_map, Data}}}.

parse_data_field(_, _, Error={error, _}) ->
    Error;
parse_data_field(<<"values">>, V, Acc) when is_binary(V) ->
    Acc#{values => V};
parse_data_field(<<"interval">>, V, Acc) when is_integer(V) ->
    Acc#{interval => V};
parse_data_field(<<"topic">>, V, Acc) when is_binary(V) ->
    Acc#{topic => V};
parse_data_field(Field, _, _) ->
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
    [values, interval, topic].

connection_info_mandatory_keys() ->
    [host, port, reconnect_timeout, data].
