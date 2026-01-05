%%%-------------------------------------------------------------------
%%% @author StayAfrica
%%% @doc
%%% Message Router
%%% Routes messages based on priority and type
%%% @end
%%%-------------------------------------------------------------------
-module(message_router).
-behaviour(gen_server).

-export([start_link/0, route_message/1, route_message/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    high_priority_queue :: queue:queue(),
    normal_priority_queue :: queue:queue(),
    low_priority_queue :: queue:queue(),
    processing :: boolean()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

route_message(Message) ->
    route_message(Message, normal).

route_message(Message, Priority) ->
    gen_server:cast(?MODULE, {route, Message, Priority}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    State = #state{
        high_priority_queue = queue:new(),
        normal_priority_queue = queue:new(),
        low_priority_queue = queue:new(),
        processing = false
    },
    %% Schedule periodic processing
    erlang:send_after(100, self(), process_queues),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({route, Message, Priority}, State) ->
    NewState = enqueue_message(Message, Priority, State),
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(process_queues, State) ->
    NewState = process_message_queues(State),
    %% Schedule next processing
    erlang:send_after(100, self(), process_queues),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

enqueue_message(Message, high, State) ->
    State#state{
        high_priority_queue = queue:in(Message, State#state.high_priority_queue)
    };
enqueue_message(Message, normal, State) ->
    State#state{
        normal_priority_queue = queue:in(Message, State#state.normal_priority_queue)
    };
enqueue_message(Message, low, State) ->
    State#state{
        low_priority_queue = queue:in(Message, State#state.low_priority_queue)
    }.

process_message_queues(State) ->
    %% Process high priority first, then normal, then low
    State1 = process_queue(high, State),
    State2 = process_queue(normal, State1),
    process_queue(low, State2).

process_queue(Priority, State) ->
    Queue = get_queue(Priority, State),
    case queue:out(Queue) of
        {{value, Message}, NewQueue} ->
            %% Process message
            spawn(fun() -> handle_message(Message) end),
            %% Update stats
            stats_collector:increment_counter(messages_processed),
            %% Update state with new queue
            set_queue(Priority, NewQueue, State);
        {empty, _} ->
            State
    end.

get_queue(high, State) -> State#state.high_priority_queue;
get_queue(normal, State) -> State#state.normal_priority_queue;
get_queue(low, State) -> State#state.low_priority_queue.

set_queue(high, Queue, State) -> State#state{high_priority_queue = Queue};
set_queue(normal, Queue, State) -> State#state{normal_priority_queue = Queue};
set_queue(low, Queue, State) -> State#state{low_priority_queue = Queue}.

handle_message(Message) ->
    %% Extract message details
    #{
        <<"conversation_id">> := ConversationId,
        <<"sender_id">> := SenderId,
        <<"receiver_id">> := ReceiverId,
        <<"text">> := Text
    } = Message,
    
    %% Store in receiver's queue
    message_queue_manager:enqueue(ReceiverId, Message),
    
    %% Persist to Django backend (use spawn_link for better error handling)
    spawn_link(fun() ->
        try
            message_persistence:persist(Message)
        catch
            Error:Reason ->
                io:format("Message persistence failed: ~p:~p~n", [Error, Reason]),
                stats_collector:increment_counter(persistence_errors)
        end
    end),
    
    %% Notify receiver if online (would integrate with WebSocket here)
    io:format("Message routed: ~p -> ~p~n", [SenderId, ReceiverId]).
