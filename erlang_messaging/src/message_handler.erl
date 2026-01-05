%%%-------------------------------------------------------------------
%%% @doc Message handler - handles sending messages
%%%-------------------------------------------------------------------
-module(message_handler).
-export([init/2]).

init(Req0, State) ->
    %% Read request body
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    %% Parse JSON
    case jsx:decode(Body, [return_maps]) of
        Message when is_map(Message) ->
            %% Extract priority
            Priority = maps:get(<<"priority">>, Message, <<"normal">>),
            PriorityAtom = binary_to_atom(Priority, utf8),
            
            %% Route message
            message_router:route_message(Message, PriorityAtom),
            
            %% Respond
            ResponseBody = jsx:encode(#{
                <<"status">> => <<"success">>,
                <<"message">> => <<"Message queued">>
            }),
            
            Req = cowboy_req:reply(200,
                #{<<"content-type">> => <<"application/json">>},
                ResponseBody,
                Req1),
            
            {ok, Req, State};
        
        _ ->
            ErrorBody = jsx:encode(#{
                <<"status">> => <<"error">>,
                <<"message">> => <<"Invalid JSON">>
            }),
            
            Req = cowboy_req:reply(400,
                #{<<"content-type">> => <<"application/json">>},
                ErrorBody,
                Req1),
            
            {ok, Req, State}
    end.
