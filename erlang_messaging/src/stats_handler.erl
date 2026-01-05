%%%-------------------------------------------------------------------
%%% @doc Stats handler - returns system statistics
%%%-------------------------------------------------------------------
-module(stats_handler).
-export([init/2]).

init(Req0, State) ->
    %% Get stats from collector
    Stats = stats_collector:get_stats(),
    
    %% Encode and respond
    ResponseBody = jsx:encode(Stats),
    
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"application/json">>},
        ResponseBody,
        Req0),
    
    {ok, Req, State}.
