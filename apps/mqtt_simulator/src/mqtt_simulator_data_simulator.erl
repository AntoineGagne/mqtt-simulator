-module(mqtt_simulator_data_simulator).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {interval :: pos_integer(),
                client_id :: term(),
                timer :: reference(),
                topic :: binary(),
                values :: queue:queue(binary())}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(ClientId, DataPoint) ->
    gen_server:start_link(?MODULE, [ClientId, DataPoint], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ClientId, #{interval := Interval, topic := Topic, values := Values}]) ->
    Ref = erlang:start_timer(Interval, self(), send),
    {ok, #state{interval = Interval,
                client_id = ClientId,
                topic = Topic,
                values = queue:from_list(Values),
                timer = Ref}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, _Ref, send}, State=#state{values = Values, interval = Interval}) ->
    NewRef = erlang:start_timer(Interval, self(), send),
    MaybeValue = queue:out(Values),
    maybe_send_value(MaybeValue, State#state{timer = NewRef});

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

maybe_send_value({empty, _}, State) ->
    {noreply, State};
maybe_send_value({{value, Value}, NewValues}, State=#state{topic = Topic}) ->
    ok = mqtt_simulator_client:publish(State#state.client_id, Topic, Value),
    {noreply, State#state{values = queue:in(Value, NewValues)}}.
