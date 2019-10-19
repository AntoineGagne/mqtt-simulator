-module(mqtt_simulator_client_config).

%% API
-export([init/0,
         fold/3,
         id/1,
         id/2,
         host/1,
         host/2,
         port/1,
         port/2,
         username/1,
         username/2,
         password/1,
         password/2,
         reconnect_timeout/1,
         reconnect_timeout/2,
         data/1,
         data/2]).

-opaque config() :: #{id := binary(),
                      host := inet:ip_address() | binary(),
                      port := inet:port_number(),
                      username => binary(),
                      password => binary(),
                      reconnect_timeout := pos_integer(),
                      data := [data()]}.
-type data() :: #{id := binary(),
                  values := [binary()],
                  interval := pos_integer(),
                  topic := binary()}.
-type no_such_field_error(Field) :: {error, {no_such_field, Field}}.

-export_type([config/0,
              data/0]).

-define(DEFAULT_RECONNECT_TIMEOUT, 5000).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> config().
init() ->
    #{id => uuid:uuid_to_string(uuid:get_v4(), binary_standard),
      host => <<"">>,
      port => 0,
      reconnect_timeout => ?DEFAULT_RECONNECT_TIMEOUT,
      data => []}.

-spec fold(fun ((Key :: term(), Value :: term(), Acc) -> Acc), Acc, config()) -> Acc.
fold(Fun, Acc, Config) ->
    maps:fold(Fun, Acc, Config).

-spec id(binary(), config()) -> config().
id(Id, Config) ->
    Config#{id := Id}.

-spec id(config()) -> binary().
id(#{id := Id}) ->
    Id.

-spec host(inet:ip_address() | binary(), config()) -> config().
host(Host, Config) ->
    Config#{host := Host}.

-spec host(config()) -> inet:ip_address() | binary().
host(#{host := Host}) ->
    Host.

-spec port(inet:port_number(), config()) -> config().
port(Port, Config) ->
    Config#{port := Port}.

-spec port(config()) -> inet:port_number().
port(#{port := Port}) ->
    Port.

-spec username(binary(), config()) -> config().
username(Username, Config) ->
    Config#{username => Username}.

-spec username(config()) -> {ok, binary()} | no_such_field_error(username).
username(#{username := Username}) ->
    {ok, Username};
username(_) ->
    {error, {no_such_field, username}}.

-spec password(binary(), config()) -> config().
password(Password, Config) ->
    Config#{password => Password}.

-spec password(config()) -> {ok, binary()} | no_such_field_error(password).
password(#{password := Password}) ->
    {ok, Password};
password(_) ->
    {error, {no_such_field, password}}.

-spec reconnect_timeout(pos_integer(), config()) -> config().
reconnect_timeout(ReconnectTimeout, Config) ->
    Config#{reconnect_timeout := ReconnectTimeout}.

-spec reconnect_timeout(config()) -> pos_integer().
reconnect_timeout(#{reconnect_timeout := ReconnectTimeout}) ->
    ReconnectTimeout.

-spec data([data()], config()) -> config().
data(Data, Config) ->
    Config#{data := Data}.

-spec data(config()) -> [data()].
data(#{data := Data}) ->
    Data.

%%%===================================================================
%%% Internal functions
%%%===================================================================
