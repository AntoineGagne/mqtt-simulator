-module(mqtt_simulator_data_simulator).

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(VIA_GPROC(Id), {via, gproc, {n, l, Id}}).

-record(state, {interval :: pos_integer(),
                client_id :: term(),
                timer :: reference(),
                topic :: binary(),
                values :: queue:queue(binary())}).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(term(), term(), mqtt_simulator_client_config:data()) ->
    {ok, pid()} | ignore | {error, term()}.
start_link(ClientId, Id, DataPoint) ->
    gen_server:start_link(?VIA_GPROC(Id), ?MODULE, [ClientId, DataPoint], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([ClientId, #{interval := Interval, topic := Topic, values := Values}]) ->
    {ok, #state{interval = Interval,
                client_id = ClientId,
                topic = Topic,
                values = queue:from_list(Values),
                timer = erlang:start_timer(Interval, self(), send)}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, _Ref, send}, State=#state{values = Values, interval = Interval}) ->
    MaybeValue = queue:out(Values),
    maybe_send_value(MaybeValue, State#state{timer = erlang:start_timer(Interval, self(), send)});

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
