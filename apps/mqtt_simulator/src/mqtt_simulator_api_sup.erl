-module(mqtt_simulator_api_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() ->
    {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    {ok, {#{strategy => one_for_one,
            intensity => 5,
            period => 10},
          [#{id => mqtt_simulator_api,
             start => {mqtt_simulator_api, start_link, []},
             restart => permanent,
             shutdown => 5000,
             type => worker,
             modules => [mqtt_simulator_api]}
          ]}}.
