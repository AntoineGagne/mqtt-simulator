-module(devices_handler).

-include_lib("kernel/include/logger.hrl").

%% API
-export([dispatch/2,
         fetch/2]).

%% cowboy callbacks
-export([init/2,
         allowed_methods/2,
         malformed_request/2,
         content_types_provided/2,
         resource_exists/2,
         delete_resource/2,
         content_types_accepted/2]).

%%%===================================================================
%%% API
%%%===================================================================

dispatch(Request=#{method := <<"POST">>}, State) ->
    update_configs(Request, State);
dispatch(Request=#{method := <<"PUT">>}, State) ->
    update_config(Request, State).

fetch(Request, State) ->
    ?LOG_INFO(#{what => fetch, state => State}),
    {<<>>, Request, State}.

%%%===================================================================
%%% cowboy callbacks
%%%===================================================================

init(Request, State) ->
    {cowboy_rest, Request, State}.

malformed_request(Request, State) ->
    {false, Request, State}.

resource_exists(Request, State) ->
    {false, Request, State}.

delete_resource(Request, State) ->
    {true, Request, State}.

content_types_provided(Request, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, fetch}], Request, State}.

allowed_methods(Request, State) ->
    {[<<"POST">>, <<"PUT">>, <<"DELETE">>, <<"GET">>], Request, State}.

content_types_accepted(Request, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, dispatch}], Request, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

update_configs(Request, Configs) ->
    ?LOG_INFO(#{what => update_configs, configs => Configs}),
    {true, Request, Configs}.

update_config(Request, Config) ->
    ?LOG_INFO(#{what => update_config, configs => Config}),
    {true, Request, Config}.
