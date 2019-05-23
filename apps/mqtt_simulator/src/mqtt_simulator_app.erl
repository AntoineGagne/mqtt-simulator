%%%-------------------------------------------------------------------
%% @doc mqtt_simulator public API
%% @end
%%%-------------------------------------------------------------------

-module(mqtt_simulator_app).

-behaviour(application).

%% Application callbacks
-export([start/2,
         stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    mqtt_simulator_sup:start_link().

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
