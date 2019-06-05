-module(mqtt_simulator_client).

-include_lib("kernel/include/logger.hrl").

-behavior(gen_statem).

-export([start_link/3,
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
               reconnect_timeout = ?RETRY_CONNECTION_MILLISECONDS :: integer()}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link(term(), term(), mqtt_simulator_client_config:config()) ->
    {ok, pid()} | {error, term()} | ignore.
start_link(Id, ConfigId, Config) ->
    gen_statem:start_link(?VIA_GPROC(Id), ?MODULE, [ConfigId, Config], []).

publish(Id, Topic, Payload) ->
    gen_statem:cast(?VIA_GPROC(Id), {publish, Topic, Payload}).

%%====================================================================
%% gen_statem callbacks
%%====================================================================

callback_mode() ->
    [handle_event_function].

init([ConfigId, Config]) ->
    process_flag(trap_exit, true),
    ReconnectTimeout = application:get_env(mqtt_simulator,
                                           reconnect_timeout,
                                           ?RETRY_CONNECTION_MILLISECONDS),
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
    Actions = [{{timeout, reconnect}, ReconnectTimeout, reconnect}, {{timeout, ping}, infinity, ping}],
    {next_state, disconnected, Data#data{client = undefined}, Actions};

handle_event(info, Message, _State, _Data) ->
    ?LOG_WARNING(#{what => unexpected_message, payload => Message}),
    keep_state_and_data;

handle_event({timeout, ping}, _, connected, #data{client = C}) ->
    pong = emqttc:ping(C),
    {keep_state_and_data, {{timeout, ping}, ?PING_TIME_MILLISECONDS, ping}};

handle_event({timeout, reconnect}, _, disconnected, #data{client = undefined}=Data) ->
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
    Host = mqtt_simulator_client_config:host(Config),
    Port = mqtt_simulator_client_config:port(Config),
    ConnectionStatus = emqttc:start_link([{host, binary_to_list(Host)},
                                          {port, Port},
                                          {keepalive, 60},
                                          {reconnect, false}]),
    handle_connection_status(ConnectionStatus, Data).

handle_connection_status({ok, Client}, Data) ->
    {next_state, disconnected, Data#data{client = Client}};
handle_connection_status(_, Data) ->
    Action = {{timeout, reconnect}, Data#data.reconnect_timeout, reconnect},
    {next_state, disconnected, Data#data{client = undefined}, Action}.
