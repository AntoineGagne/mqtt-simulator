-module(mqtt_simulator_data_simulators_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(VIA_GPROC(Id), {via, gproc, {n, l, Id}}).
-define(SERVER, ?MODULE).

-spec start_link(term(), term()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Id, ClientId) ->
        supervisor:start_link(?VIA_GPROC(Id), ?MODULE, [ClientId]).

init([ClientId]) ->
    {ok, {#{strategy => simple_one_for_one,
            intensity => 5,
            period => 10},
          [#{id => mqtt_simulator_data_simulator,
             start => {mqtt_simulator_data_simulator, start_link, [ClientId]},
             restart => transient,
             shutdown => 5000,
             type => worker,
             modules => [mqtt_simulator_data_simulator]}]}}.
