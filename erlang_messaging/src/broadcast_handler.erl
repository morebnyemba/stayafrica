%%%-------------------------------------------------------------------
%%% @doc Broadcast handler - sends message to multiple users
%%%-------------------------------------------------------------------
-module(broadcast_handler).
-export([init/2]).

init(Req0, State) ->
    %% Read request body
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    %% Parse JSON
    case jsx:decode(Body, [return_maps]) of
        #{<<"message">> := Message, <<"user_ids">> := UserIds} when is_list(UserIds) ->
            %% Broadcast to all users
            lists:foreach(fun(UserId) ->
                UserMessage = Message#{<<"receiver_id">> => UserId},
                message_router:route_message(UserMessage, normal)
            end, UserIds),
            
            ResponseBody = jsx:encode(#{
                <<"status">> => <<"success">>,
                <<"message">> => <<"Broadcast queued">>,
                <<"recipients">> => length(UserIds)
            }),
            
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                ResponseBody,
                Req1),
            
            {ok, Req, State};
        
        _ ->
            ErrorBody = jsx:encode(#{
                <<"status">> => <<"error">>,
                <<"message">> => <<"Invalid request format">>
            }),
            
            Req = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                ErrorBody,
                Req1),
            
            {ok, Req, State}
    end.
