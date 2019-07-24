-module(mqtt_simulator_api).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2]).

-define(SERVER, ?MODULE).
-define(DEFAULT_PORT, 8000).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    Port = application:get_env(mqtt_simulator, api_port, ?DEFAULT_PORT),
    Routes = [{"/devices/", mqtt_simulator_devices_handler, []},
              {"/devices/:id", [{id, nonempty}], mqtt_simulator_devices_handler, []}],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    {ok, _} = cowboy:start_clear(?SERVER,
                                 [{port, Port}],
                                 #{env => #{dispatch => Dispatch}}
                                ),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    cowboy:stop_listener(?SERVER).

%%%===================================================================
%%% Internal functions
%%%===================================================================
