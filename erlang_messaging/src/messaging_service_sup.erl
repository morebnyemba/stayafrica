%%%-------------------------------------------------------------------
%%% @author StayAfrica
%%% @doc
%%% Messaging Service Supervisor
%%% Implements OTP supervisor pattern for fault tolerance
%%% @end
%%%-------------------------------------------------------------------
-module(messaging_service_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },
    
    %% Child specifications
    ChildSpecs = [
        %% Message router - routes messages to appropriate handlers
        #{
            id => message_router,
            start => {message_router, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [message_router]
        },
        
        %% Message queue manager - manages per-user message queues
        #{
            id => message_queue_manager,
            start => {message_queue_manager, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [message_queue_manager]
        },
        
        %% Message persistence - persists messages to Django backend
        #{
            id => message_persistence,
            start => {message_persistence, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [message_persistence]
        },
        
        %% Stats collector - collects and reports metrics
        #{
            id => stats_collector,
            start => {stats_collector, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [stats_collector]
        }
    ],
    
    {ok, {SupFlags, ChildSpecs}}.
