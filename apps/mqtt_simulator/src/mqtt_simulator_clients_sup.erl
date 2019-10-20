-module(mqtt_simulator_clients_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_client/1,
         update_client/1,
         stop_client/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-define(WHERE(Id), gproc:where({n, l, Id})).
-define(CLIENT_SUP_ID(Id), {client_sup, Id}).

-type child() :: undefined | pid().

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

-spec start_client(mqtt_simulator_client_config:config()) ->
    {ok, child()} | {ok, child(), term()} | {error, term()}.
start_client(Config) ->
    Id = mqtt_simulator_client_config:id(Config),
    supervisor:start_child(?SERVER, [?CLIENT_SUP_ID(Id), Config]).

-spec update_client(mqtt_simulator_client_config:config()) -> ok | {error, term()}.
update_client(Config) ->
    mqtt_simulator_client_sup:update_config(Config).

-spec stop_client(pid()) -> ok | {error, term()}.
stop_client(Pid) ->
    supervisor:terminate_child(?SERVER, Pid).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    {ok, {#{strategy => simple_one_for_one,
            intensity => 5,
            period => 10},
          [#{id => mqtt_simulator_client_sup,
             start => {mqtt_simulator_client_sup, start_link, []},
             restart => temporary,
             shutdown => 5000,
             type => supervisor,
             modules => [mqtt_simulator_client_sup]}]}}.

%%====================================================================
%% Internal functions
%%====================================================================
