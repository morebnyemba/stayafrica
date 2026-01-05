%%%-------------------------------------------------------------------
%%% @author StayAfrica
%%% @doc
%%% Messaging Service Application
%%% High-performance, fault-tolerant messaging service using Erlang/OTP
%%% @end
%%%-------------------------------------------------------------------
-module(messaging_service_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    io:format("Starting StayAfrica Messaging Service...~n"),
    
    %% Get configuration
    {ok, Port} = application:get_env(messaging_service, http_port),
    
    %% Start the supervisor
    case messaging_service_sup:start_link() of
        {ok, Pid} ->
            %% Start HTTP API
            start_http_server(Port),
            io:format("Messaging Service started on port ~p~n", [Port]),
            {ok, Pid};
        Error ->
            Error
    end.

stop(_State) ->
    io:format("Stopping StayAfrica Messaging Service...~n"),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

start_http_server(Port) ->
    %% Define routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/health", health_handler, []},
            {"/api/messages/send", message_handler, []},
            {"/api/messages/queue/:user_id", queue_handler, []},
            {"/api/messages/broadcast", broadcast_handler, []},
            {"/api/stats", stats_handler, []}
        ]}
    ]),
    
    %% Start HTTP listener
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, Port}],
        #{env => #{dispatch => Dispatch}}
    ),
    ok.
