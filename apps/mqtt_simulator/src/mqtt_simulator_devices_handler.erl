-module(mqtt_simulator_devices_handler).

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
    ?LOG_DEBUG(#{what => update_configs, method => post}),
    update_configs(Request, State);
dispatch(Request=#{method := <<"PUT">>}, State) ->
    ?LOG_DEBUG(#{what => update_config, method => put}),
    update_config(Request, State).

fetch(Request, State) ->
    {ok, Response} = mqtt_simulator_config_encoder:encode(State),
    {ok, Encoded} = jsone:try_encode(Response),
    {Encoded, Request, State}.

%%%===================================================================
%%% cowboy callbacks
%%%===================================================================

init(Request, State) ->
    {cowboy_rest, Request, State}.

malformed_request(Request=#{method := <<"POST">>}, _State) ->
    {ok, Body, Request2} = cowboy_req:read_body(Request),
    MaybeDecoded = jsone:try_decode(Body),
    Result = maybe_parse_body(MaybeDecoded, fun parse_configs/1),
    handle_parse_result(Result, Request2);
malformed_request(Request=#{method := <<"PUT">>}, _State) ->
    {ok, Body, Request2} = cowboy_req:read_body(Request),
    MaybeDecoded = jsone:try_decode(Body),
    Result = maybe_parse_body(MaybeDecoded, fun mqtt_simulator_config_parser:parse/1),
    handle_parse_result(Result, Request2);
malformed_request(Request, State) ->
    {false, Request, State}.

resource_exists(Request, State) ->
    Id = cowboy_req:binding(id, Request),
    handle_resource_exists(Id, Request, State).

delete_resource(Request, State) ->
    {true, Request, State}.

content_types_provided(Request, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, fetch}], Request, State}.

allowed_methods(Request, State) ->
    {[<<"POST">>, <<"PUT">>, <<"DELETE">>, <<"GET">>], Request, State}.

content_types_accepted(Request, State) ->
    ?LOG_DEBUG(#{what => update, request => Request, state => State}),
    {[{{<<"application">>, <<"json">>, '*'}, dispatch}], Request, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_resource_exists(undefined, Request=#{method := <<"GET">>}, _State) ->
    ?LOG_INFO(#{what => fetch_config, method => get}),
    Configs = mqtt_simulator_clients_config:get_configs(),
    {true, Request, Configs};
handle_resource_exists(undefined, Request, State) ->
    {true, Request, State};
handle_resource_exists(Id, Request=#{method := <<"GET">>}, State) ->
    ?LOG_INFO(#{what => fetch_config, id => Id, method => get}),
    Return = mqtt_simulator_clients_config:get_config(Id),
    handle_get_config(Return, Request, State);
handle_resource_exists(_Id, Request, State) ->
    {true, Request, State}.

handle_get_config({error, {not_found, Id}}, Request, State) ->
    ?LOG_ERROR(#{what => fetch_config, status => not_found, id => Id}),
    {false, Request, State};
handle_get_config(Config, Request, _State) ->
    {true, Request, Config}.

maybe_parse_body({ok, Decoded, _}, Parser) ->
    Parser(Decoded);
maybe_parse_body(Error={error, _}, _) ->
    Error.

handle_parse_result(Result={ok, _}, Request) ->
    {false, Request, Result};
handle_parse_result(Reason={error, _}, Request=#{method := Method}) ->
    ?LOG_ERROR(#{what => malformed_request, reason => Reason, method => Method}),
    {true, Request, Reason}.

update_configs(Request, {ok, Configs}) ->
    ?LOG_DEBUG(#{what => update_configs, configs => Configs}),
    Return = mqtt_simulator_clients_config:update_configs(Configs),
    {ok, Encoded} = mqtt_simulator_config_encoder:encode(Return),
    {ok, Binary} = jsone:try_encode(Encoded),
    {true, cowboy_req:set_resp_body(Binary, Request), Configs}.

parse_configs(Decoded) when is_list(Decoded) ->
    lists:foldl(fun parse_config/2, {ok, []}, Decoded);
parse_configs(_) ->
    {error, invalid_format}.

parse_config(_, Error={error, _}) ->
    Error;
parse_config(Config, {ok, Configs}) ->
    case mqtt_simulator_config_parser:parse(Config) of
        {ok, Parsed} -> {ok, [Parsed | Configs]};
        Error -> Error
    end.

update_config(Request, {ok, Config}) ->
    Id = cowboy_req:binding(id, Request),
    ?LOG_DEBUG(#{what => update_config, config => Config, id => Id}),
    UpdatedConfig = mqtt_simulator_client_config:id(Id, Config),
    case mqtt_simulator_clients_config:update_config(UpdatedConfig) of
        ok ->
            {ok, Encoded} = mqtt_simulator_config_encoder:encode(UpdatedConfig),
            {ok, Binary} = jsone:try_encode(Encoded),
            {true, cowboy_req:set_resp_body(Binary, Request), Config};
        {error, _} ->
            {false, Request, Config}
    end.
