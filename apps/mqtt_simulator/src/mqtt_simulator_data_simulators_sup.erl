-module(mqtt_simulator_data_simulators_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
        supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok, {#{strategy => simple_one_for_one,
            intensity => 5,
            period => 10},
          [#{id => mqtt_simulator_data_simulator,
             start => {mqtt_simulator_data_simulator, start_link, []},
             restart => transient,
             shutdown => 5000,
             type => worker,
             modules => [mqtt_simulator_data_simulator]}]}}.
