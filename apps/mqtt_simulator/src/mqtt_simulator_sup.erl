%%%-------------------------------------------------------------------
%% @doc mqtt_simulator top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mqtt_simulator_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(DEFAULT_SYNCHRONIZATION_INTERVAL, 60000).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {#{strategy => one_for_one,
            intensity => 5,
            period => 10},
          [#{id => mqtt_simulator_clients_config,
             start => {mqtt_simulator_clients_config, start_link,
                       [?DEFAULT_SYNCHRONIZATION_INTERVAL]},
             restart => permanent,
             shutdown => 5000,
             type => worker,
             modules => [mqtt_simulator_clients_config]},
           #{id => mqtt_simulator_clients_sup,
             start => {mqtt_simulator_clients_sup, start_link, []},
             restart => permanent,
             shutdown => 5000,
             type => worker,
             modules => [mqtt_simulator_clients_sup]}]}}.

%%====================================================================
%% Internal functions
%%====================================================================
