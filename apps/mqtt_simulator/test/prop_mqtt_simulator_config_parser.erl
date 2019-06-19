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

%%%===================================================================
%%% Generators
%%%===================================================================

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

values() ->
    list(ascii_binary()).

ascii_binary() ->
    ?LET(C, ascii_character(),
         ?LET(S, non_empty(list(C)),
             list_to_binary(S))).

ascii_character() ->
    range(0, 127).
