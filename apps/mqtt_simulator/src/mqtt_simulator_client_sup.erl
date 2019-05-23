-module(mqtt_simulator_client_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(VIA_GPROC(Id), {via, gproc, {n, l, Id}}).
-define(CLIENT_ID(Id), {client, Id}).
-define(DATA_SIMULATOR_SUP_ID(Id), {data_simulator_sup_id, Id}).
-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link(term(), mqtt_simulator_clients_config:config()) ->
    {ok, pid()} | ignore | {error, term()}.
start_link(Id, Config) ->
    supervisor:start_link(?VIA_GPROC(Id), ?MODULE, [Config]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([Config]) ->
    Id = mqtt_simulator_client_config:id(Config),
    ClientId = ?CLIENT_ID(Id),
    {ok, {#{strategy => one_for_all,
            intensity => 5,
            period => 10},
          [#{id => mqtt_simulator_data_simulators_sup,
             start => {mqtt_simulator_data_simulators_sup, start_link,
                       [?DATA_SIMULATOR_SUP_ID(Id), ClientId]},
             restart => permanent,
             shutdown => 5000,
             type => supervisor,
             modules => [mqtt_simulator_data_simulators_sup]},
           #{id => mqtt_simulator_client,
             start => {mqtt_simulator_client, start_link, [ClientId, Config]},
             restart => permanent,
             shutdown => 5000,
             type => worker,
             modules => [mqtt_simulator_client]}]}}.

%%====================================================================
%% Internal functions
%%====================================================================
