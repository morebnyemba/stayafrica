%%%-------------------------------------------------------------------
%%% @author StayAfrica
%%% @doc
%%% Message Queue Manager
%%% Manages per-user message queues with persistence
%%% @end
%%%-------------------------------------------------------------------
-module(message_queue_manager).
-behaviour(gen_server).

-export([start_link/0, enqueue/2, dequeue/1, get_queue_size/1, clear_queue/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    queues :: map()  %% UserId -> queue:queue()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

enqueue(UserId, Message) ->
    gen_server:cast(?MODULE, {enqueue, UserId, Message}).

dequeue(UserId) ->
    gen_server:call(?MODULE, {dequeue, UserId}).

get_queue_size(UserId) ->
    gen_server:call(?MODULE, {queue_size, UserId}).

clear_queue(UserId) ->
    gen_server:cast(?MODULE, {clear, UserId}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{queues = maps:new()}}.

handle_call({dequeue, UserId}, _From, State) ->
    Queues = State#state.queues,
    case maps:get(UserId, Queues, queue:new()) of
        Queue when is_tuple(Queue) ->
            case queue:out(Queue) of
                {{value, Message}, NewQueue} ->
                    NewQueues = maps:put(UserId, NewQueue, Queues),
                    {reply, {ok, Message}, State#state{queues = NewQueues}};
                {empty, _} ->
                    {reply, {error, empty}, State}
            end;
        _ ->
            {reply, {error, empty}, State}
    end;

handle_call({queue_size, UserId}, _From, State) ->
    Queues = State#state.queues,
    Queue = maps:get(UserId, Queues, queue:new()),
    Size = queue:len(Queue),
    {reply, {ok, Size}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({enqueue, UserId, Message}, State) ->
    Queues = State#state.queues,
    Queue = maps:get(UserId, Queues, queue:new()),
    
    %% Check max queue size
    {ok, MaxSize} = application:get_env(messaging_service, message_queue_max_size),
    NewQueue = case queue:len(Queue) >= MaxSize of
        true ->
            %% Drop oldest message if queue is full
            {{value, _}, TempQueue} = queue:out(Queue),
            queue:in(Message, TempQueue);
        false ->
            queue:in(Message, Queue)
    end,
    
    NewQueues = maps:put(UserId, NewQueue, Queues),
    {noreply, State#state{queues = NewQueues}};

handle_cast({clear, UserId}, State) ->
    Queues = State#state.queues,
    NewQueues = maps:remove(UserId, Queues),
    {noreply, State#state{queues = NewQueues}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
