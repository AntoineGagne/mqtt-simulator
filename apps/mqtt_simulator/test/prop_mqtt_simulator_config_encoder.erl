-module(prop_mqtt_simulator_config_encoder).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

prop_valid_config() ->
    ?FORALL(Config, valid_config(),
            begin
                 {ok, Parsed} = mqtt_simulator_config_encoder:encode(Config),
                 is_map(Parsed) andalso map_size(Parsed) =/= 0
            end).

prop_missing_config_key() ->
    ?FORALL(Config, config_with_missing_key(),
            begin
                 {error, {missing_keys, Keys}} = mqtt_simulator_config_encoder:encode(Config),
                 is_list(Keys) andalso length(Keys) > 0
            end).

prop_unknown_config_key() ->
    ?FORALL({Config, Key, Value},
            {valid_config(), unknown_key(mandatory_config_keys()), ascii_binary()},
            begin
                RawConfig = Config#{Key => Value},
                 {error, {invalid_field, Invalid}} = mqtt_simulator_config_encoder:encode(RawConfig),
                 equals(Invalid, Key)
            end).

prop_missing_data_keys() ->
    ?FORALL(Config, config_with_data_with_missing_keys(),
            begin
                 {error, {missing_keys, Keys}} = mqtt_simulator_config_encoder:encode(Config),
                 is_list(Keys) andalso length(Keys) > 0
            end).

prop_unknown_data_key() ->
    ?FORALL({Config, Key, Value},
            {valid_config(), unknown_key(mandatory_config_keys()), ascii_binary()},
            begin
                Data = mqtt_simulator_client_config:data(Config),
                DataWithUnknownKey = lists:map(fun (D) -> D#{Key => Value} end, Data),
                UpdatedConfig = mqtt_simulator_client_config:data(DataWithUnknownKey, Config),
                 {error, {invalid_field, Invalid}} = mqtt_simulator_config_encoder:encode(UpdatedConfig),
                 equals(Invalid, Key)
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
    ?LET({Id, Host, Port, Username, Password, ReconnectTimeout, Data},
         {ascii_binary(),
          ascii_binary(),
          pos_integer(),
          ascii_binary(),
          ascii_binary(),
          pos_integer(),
          non_empty(list(valid_data()))},
         begin
             C1 = mqtt_simulator_client_config:init(),
             C2 = mqtt_simulator_client_config:host(Host, C1),
             C3 = mqtt_simulator_client_config:port(Port, C2),
             C4 = mqtt_simulator_client_config:username(Username, C3),
             C5 = mqtt_simulator_client_config:password(Password, C4),
             C6 = mqtt_simulator_client_config:reconnect_timeout(ReconnectTimeout, C5),
             C7 = mqtt_simulator_client_config:data(Data, C6),
             mqtt_simulator_client_config:id(Id, C7)
         end).

valid_data() ->
    ?LET({Id, Interval, Topic, Values}, {ascii_binary(), pos_integer(), ascii_binary(), values()},
         #{id => Id,
           topic => Topic,
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
    [host, port, reconnect_timeout, data].

mandatory_data_keys() ->
    [interval, topic, values].

%%%===================================================================
%%% Generators
%%%===================================================================
