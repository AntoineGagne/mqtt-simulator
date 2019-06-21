-module(mqtt_simulator_client).

-include_lib("kernel/include/logger.hrl").

-behavior(gen_statem).

-export([start_link/3,
         update_config/2,
         publish/3]).

-export([callback_mode/0,
         init/1,
         handle_event/4,
         terminate/3]).

-define(VIA_GPROC(Id), {via, gproc, {n, l, Id}}).
-define(PING_TIME_MILLISECONDS, 60000).
-define(RETRY_CONNECTION_MILLISECONDS, 5000).

-record(data, {client = undefined :: pid() | undefined,
               config_id :: term(),
               config :: mqtt_simulator_client_config:config(),
               reconnect_timeout :: integer()}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link(term(), term(), mqtt_simulator_client_config:config()) ->
    {ok, pid()} | {error, term()} | ignore.
start_link(Id, ConfigId, Config) ->
    gen_statem:start_link(?VIA_GPROC(Id), ?MODULE, [ConfigId, Config], []).

update_config(Id, Config) ->
    gen_statem:cast(?VIA_GPROC(Id), {update_config, Config}).

-spec publish(term(), binary(), binary()) -> ok.
publish(Id, Topic, Payload) ->
    gen_statem:cast(?VIA_GPROC(Id), {publish, Topic, Payload}).

%%====================================================================
%% gen_statem callbacks
%%====================================================================

callback_mode() ->
    [handle_event_function].

init([ConfigId, Config]) ->
    process_flag(trap_exit, true),
    ReconnectTimeout = mqtt_simulator_client_config:reconnect_timeout(Config),
    self() ! connect,
    {ok, disconnected, #data{config_id = ConfigId,
                             config = Config,
                             reconnect_timeout = ReconnectTimeout}}.

handle_event(cast, {publish, Topic, Payload}, connected, #data{client = Client}) ->
    emqttc:publish(Client, Topic, Payload),
    keep_state_and_data;
handle_event(cast, {publish, Topic, Payload}, disconnected, _Data) ->
    ?LOG_WARNING(#{what => publish_error, topic => Topic, payload => Payload,
                   reason => disconnected}),
    keep_state_and_data;

handle_event(info, connect, disconnected, Data=#data{config = Config, config_id = ConfigId}) ->
    DataPoints = mqtt_simulator_client_config:data(Config),
    ok = mqtt_simulator_data_simulators_config:update_config(ConfigId, DataPoints),
    try_connect(Data);

handle_event(info, {mqttc, _Client, connected}, disconnected, Data=#data{client = _Client}) ->
    ?LOG_DEBUG(#{what => mqtt_client_connection, status => connected}),
    {next_state, connected, Data, {{timeout, ping}, ?PING_TIME_MILLISECONDS, ping}};

handle_event(info, {mqttc, _Client,  disconnected}, connected, Data=#data{client = _Client}) ->
    ?LOG_DEBUG(#{what => mqtt_client_connection, status => disconnected}),
    {next_state, disconnected, Data, {{timeout, ping}, infinity, ping}};

handle_event(info, {'EXIT', _Client,  _}, _,
             Data=#data{client = _Client, reconnect_timeout = ReconnectTimeout}) ->
    Actions = [{{timeout, reconnect}, ReconnectTimeout, reconnect},
               {{timeout, ping}, infinity, ping}],
    {next_state, disconnected, Data#data{client = undefined}, Actions};

handle_event(info, Message, _State, _Data) ->
    ?LOG_WARNING(#{what => unexpected_message, payload => Message}),
    keep_state_and_data;

handle_event({timeout, ping}, _, connected, #data{client = Client}) ->
    pong = emqttc:ping(Client),
    {keep_state_and_data, {{timeout, ping}, ?PING_TIME_MILLISECONDS, ping}};

handle_event({timeout, reconnect}, _, disconnected, Data=#data{client = undefined}) ->
    ?LOG_INFO(#{what => reconnect}),
    try_connect(Data);

handle_event(EventType, Content, State, Data) ->
    ?LOG_WARNING(#{what => unexpected_event, type => EventType, content => Content, state => State,
                   data => Data}),
    keep_state_and_data.

terminate(Reason, _, _) ->
    ?LOG_INFO(#{what => terminated, reason => Reason}).

%%====================================================================
%% Internal functions
%%====================================================================

try_connect(Data=#data{config = Config}) ->
    DefaultConfig = default_config(),
    AdaptedConfig = to_mqttc_config(Config),
    ConnectionStatus = emqttc:start_link(DefaultConfig ++ AdaptedConfig),
    handle_connection_status(ConnectionStatus, Data).

default_config() ->
    [{logger, none},
     {keepalive, 60},
     {reconnect, false}].

to_mqttc_config(Config) ->
    lists:filtermap(fun (Fun) -> adapt_config(Fun, Config) end,
                    [{host, fun mqtt_simulator_client_config:host/1},
                     {port, fun mqtt_simulator_client_config:port/1},
                     {username, fun mqtt_simulator_client_config:username/1},
                     {password, fun mqtt_simulator_client_config:password/1}]).

adapt_config({host, Fun}, Config) ->
    {true, {host, binary_to_list(Fun(Config))}};
adapt_config({port, Fun}, Config) ->
    {true, {port, Fun(Config)}};
adapt_config({username, Fun}, Config) ->
    adapt_optional_field({username, Fun(Config)});
adapt_config({password, Fun}, Config) ->
    adapt_optional_field({password, Fun(Config)}).

adapt_optional_field({Field, {ok, Value}}) ->
    {true, {Field, Value}};
adapt_optional_field(_) ->
    false.

handle_connection_status({ok, Client}, Data) ->
    {next_state, disconnected, Data#data{client = Client}};
handle_connection_status(_, Data) ->
    Action = {{timeout, reconnect}, Data#data.reconnect_timeout, reconnect},
    {next_state, disconnected, Data#data{client = undefined}, Action}.
