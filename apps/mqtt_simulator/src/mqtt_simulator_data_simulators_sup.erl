-module(mqtt_simulator_data_simulators_sup).

-behaviour(supervisor).

%% API
-export([start_link/2,
         start_data_simulator/2,
         stop_data_simulator/2]).

%% Supervisor callbacks
-export([init/1]).

-define(DATA_SIMULATOR_ID(Id), {data_simulator, Id}).
-define(VIA_GPROC(Id), {via, gproc, {n, l, Id}}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(term(), term()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Id, ClientId) ->
        supervisor:start_link(?VIA_GPROC(Id), ?MODULE, [ClientId]).

start_data_simulator(Id, DataPoint) ->
    #{id := DataPointId} = DataPoint,
    supervisor:start_child(?VIA_GPROC(Id), [?DATA_SIMULATOR_ID(DataPointId), DataPoint]).

stop_data_simulator(Id, Pid) ->
    supervisor:terminate_child(?VIA_GPROC(Id), Pid).

%%%===================================================================
%%% supervisor callbacks
%%%===================================================================

init([ClientId]) ->
    {ok, {#{strategy => simple_one_for_one,
            intensity => 5,
            period => 10},
          [#{id => mqtt_simulator_data_simulator,
             start => {mqtt_simulator_data_simulator, start_link, [ClientId]},
             restart => temporary,
             shutdown => 5000,
             type => worker,
             modules => [mqtt_simulator_data_simulator]}]}}.
