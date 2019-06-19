-module(prop_mqtt_simulator_config_parser).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

prop_valid_config() ->
    ?FORALL(Config, valid_config(),
            begin
                 RawConfig = jsone:decode(jsone:encode(Config)),
                 {ok, Parsed} = mqtt_simulator_config_parser:parse(RawConfig),
                 is_map(Parsed) andalso map_size(Parsed) =/= 0
            end).

prop_missing_config_key() ->
    ?FORALL(Config, config_with_missing_key(),
            begin
                 RawConfig = jsone:decode(jsone:encode(Config)),
                 {error, {missing_keys, Keys}} = mqtt_simulator_config_parser:parse(RawConfig),
                 is_list(Keys) andalso length(Keys) > 0
            end).

prop_unknown_config_key() ->
    ?FORALL({Config, Key, Value},
            {valid_config(), unknown_key(mandatory_config_keys()), ascii_binary()},
            begin
                RawConfig = jsone:decode(jsone:encode(Config#{Key => Value})),
                 {error, {invalid_field, Invalid}} = mqtt_simulator_config_parser:parse(RawConfig),
                 equals(Invalid, atom_to_binary(Key, utf8))
            end).

prop_missing_data_keys() ->
    ?FORALL(Config, config_with_data_with_missing_keys(),
            begin
                 RawConfig = jsone:decode(jsone:encode(Config)),
                 {error, {missing_keys, Keys}} = mqtt_simulator_config_parser:parse(RawConfig),
                 is_list(Keys) andalso length(Keys) > 0
            end).

prop_unknown_data_key() ->
    ?FORALL({Config, Key, Value},
            {valid_config(), unknown_key(mandatory_config_keys()), ascii_binary()},
            begin
                Data = mqtt_simulator_client_config:data(Config),
                DataWithUnknownKey = lists:map(fun (D) -> D#{Key => Value} end, Data),
                UpdatedConfig = mqtt_simulator_client_config:data(DataWithUnknownKey, Config),
                RawConfig = jsone:decode(jsone:encode(UpdatedConfig)),
                 {error, {invalid_field, Invalid}} = mqtt_simulator_config_parser:parse(RawConfig),
                 equals(Invalid, atom_to_binary(Key, utf8))
            end).

%%%===================================================================
%%% Generators
%%%===================================================================

config_with_missing_key() ->
    ?LET({Config, Keys}, {valid_config(), keys(mandatory_config_keys())},
         maps:without(Keys, Config)).

config_with_data_with_missing_keys() ->
    ?LET({Config, Keys}, {valid_config(), keys(mandatory_data_keys())},
         begin
             Data = mqtt_simulator_client_config:data(Config),
             DataWithMissingKeys = lists:map(fun (D) -> maps:without(Keys, D) end, Data),
             mqtt_simulator_client_config:data(DataWithMissingKeys, Config)
         end).

valid_config() ->
    ?LET({Host, Port, Username, Password, ReconnectTimeout, Data},
         {ascii_binary(), pos_integer(), ascii_binary(), ascii_binary(), pos_integer(), non_empty(list(valid_data()))},
         #{host => Host,
           port => Port,
           username => Username,
           password => Password,
           reconnectTimeout => ReconnectTimeout,
           data => Data}).

valid_data() ->
    ?LET({Interval, Topic, Values}, {pos_integer(), ascii_binary(), values()},
         #{topic => Topic,
           interval => Interval,
           values => Values}).

unknown_key(Keys) ->
    ?SUCHTHAT(Atom, atom(), not lists:member(Atom, Keys)).

keys(Keys) ->
    non_empty(list(key(Keys))).

key(Keys) ->
    oneof(Keys).

values() ->
    list(ascii_binary()).

ascii_binary() ->
    ?LET(C, ascii_character(),
         ?LET(S, non_empty(list(C)),
             list_to_binary(S))).

ascii_character() ->
    range(0, 127).

mandatory_config_keys() ->
    [host, port, reconnectTimeout, data].

mandatory_data_keys() ->
    [interval, topic, values].
