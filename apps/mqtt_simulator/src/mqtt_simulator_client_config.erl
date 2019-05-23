-module(mqtt_simulator_client_config).

%% API
-export([init/0,
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

-opaque config() :: #{id := term(),
                      host := inet:ip_address() | string(),
                      port := inet:port_number(),
                      username => binary(),
                      password => binary(),
                      reconnect_timeout := pos_integer(),
                      data := [data()]}.
-type data() :: #{values := [term()],
                  interval := pos_integer(),
                  topic := binary()}.
-type no_such_field_error(Field) :: {error, {no_such_field, Field}}.

-export_type([config/0]).

-define(DEFAULT_RECONNECT_TIMEOUT, 60000).

%%%===================================================================
%%% API
%%%===================================================================

-spec init() -> config().
init() ->
    #{id => <<"">>,
      host => "",
      port => 0,
      reconnect_timeout => ?DEFAULT_RECONNECT_TIMEOUT,
      data => []}.

-spec id(term(), config()) -> config().
id(Id, Config) ->
    Config#{id := Id}.

-spec id(config()) -> term().
id(#{id := Id}) ->
    Id.

-spec host(inet:ip_address() | string(), config()) -> config().
host(Host, Config) ->
    Config#{host := Host}.

-spec host(config()) -> inet:ip_address() | string().
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
