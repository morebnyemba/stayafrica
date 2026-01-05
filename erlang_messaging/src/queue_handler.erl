%%%-------------------------------------------------------------------
%%% @doc Queue handler - retrieves messages from user queue
%%%-------------------------------------------------------------------
-module(queue_handler).
-export([init/2]).

init(Req0, State) ->
    %% Get user_id from path
    UserId = cowboy_req:binding(user_id, Req0),
    
    %% Get messages from queue
    Messages = get_user_messages(UserId, 10),
    
    ResponseBody = jsx:encode(#{
        <<"status">> => <<"success">>,
        <<"user_id">> => UserId,
        <<"messages">> => Messages,
        <<"count">> => length(Messages)
    }),
    
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        ResponseBody,
        Req0),
    
    {ok, Req, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_user_messages(UserId, MaxCount) ->
    get_user_messages(UserId, MaxCount, []).

get_user_messages(_UserId, 0, Acc) ->
    lists:reverse(Acc);
get_user_messages(UserId, Count, Acc) ->
    case message_queue_manager:dequeue(UserId) of
        {ok, Message} ->
            get_user_messages(UserId, Count - 1, [Message | Acc]);
        {error, empty} ->
            lists:reverse(Acc)
    end.
