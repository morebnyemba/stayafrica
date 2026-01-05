%%%-------------------------------------------------------------------
%%% @author StayAfrica
%%% @doc
%%% Message Persistence
%%% Persists messages to Django backend via HTTP API
%%% @end
%%%-------------------------------------------------------------------
-module(message_persistence).
-behaviour(gen_server).

-export([start_link/0, persist/1, persist_batch/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    batch_queue :: queue:queue(),
    batch_size :: integer(),
    django_url :: string()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

persist(Message) ->
    gen_server:cast(?MODULE, {persist, Message}).

persist_batch(Messages) ->
    gen_server:cast(?MODULE, {persist_batch, Messages}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, DjangoUrl} = application:get_env(messaging_service, django_api_url),
    State = #state{
        batch_queue = queue:new(),
        batch_size = 10,
        django_url = DjangoUrl
    },
    %% Schedule periodic batch persistence
    erlang:send_after(5000, self(), flush_batch),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({persist, Message}, State) ->
    %% Add to batch queue
    NewQueue = queue:in(Message, State#state.batch_queue),
    NewState = State#state{batch_queue = NewQueue},
    
    %% If batch is full, flush immediately
    case queue:len(NewQueue) >= State#state.batch_size of
        true ->
            flush_batch_sync(NewState),
            {noreply, NewState#state{batch_queue = queue:new()}};
        false ->
            {noreply, NewState}
    end;

handle_cast({persist_batch, Messages}, State) ->
    %% Send batch to Django
    spawn(fun() -> send_to_django(Messages, State#state.django_url) end),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(flush_batch, State) ->
    NewState = flush_batch_sync(State),
    %% Schedule next flush
    erlang:send_after(5000, self(), flush_batch),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    %% Flush any remaining messages
    flush_batch_sync(State),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

flush_batch_sync(State) ->
    Queue = State#state.batch_queue,
    case queue:is_empty(Queue) of
        true ->
            State;
        false ->
            Messages = queue:to_list(Queue),
            spawn(fun() -> send_to_django(Messages, State#state.django_url) end),
            State#state{batch_queue = queue:new()}
    end.

send_to_django(Messages, DjangoUrl) ->
    %% Convert messages to JSON
    JsonMessages = lists:map(fun message_to_json/1, Messages),
    Body = jsx:encode(#{<<"messages">> => JsonMessages}),
    
    %% Send HTTP POST to Django
    Url = DjangoUrl ++ "/api/messaging/erlang/persist/",
    Headers = [
        {"Content-Type", "application/json"},
        {"X-Erlang-Service", "messaging"}
    ],
    
    case httpc:request(post, {Url, Headers, "application/json", Body}, [], []) of
        {ok, {{_, 200, _}, _, _}} ->
            io:format("Persisted ~p messages to Django~n", [length(Messages)]),
            stats_collector:increment_counter(messages_persisted, length(Messages)),
            ok;
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            io:format("Django persistence failed: ~p - ~p~n", [StatusCode, ResponseBody]),
            stats_collector:increment_counter(persistence_failures),
            error;
        {error, Reason} ->
            io:format("Django persistence error: ~p~n", [Reason]),
            stats_collector:increment_counter(persistence_errors),
            error
    end.

message_to_json(Message) ->
    %% Convert Erlang map to JSON-compatible format
    maps:fold(fun(K, V, Acc) ->
        maps:put(ensure_binary(K), ensure_binary(V), Acc)
    end, #{}, Message).

ensure_binary(V) when is_binary(V) -> V;
ensure_binary(V) when is_list(V) -> list_to_binary(V);
ensure_binary(V) when is_integer(V) -> V;
ensure_binary(V) when is_float(V) -> V;
ensure_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
ensure_binary(V) -> V.
