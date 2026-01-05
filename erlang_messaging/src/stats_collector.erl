%%%-------------------------------------------------------------------
%%% @author StayAfrica
%%% @doc
%%% Stats Collector
%%% Collects and reports system metrics
%%% @end
%%%-------------------------------------------------------------------
-module(stats_collector).
-behaviour(gen_server).

-export([start_link/0, increment_counter/1, increment_counter/2, get_stats/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    counters :: map(),
    start_time :: integer()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

increment_counter(Counter) ->
    increment_counter(Counter, 1).

increment_counter(Counter, Amount) ->
    gen_server:cast(?MODULE, {increment, Counter, Amount}).

get_stats() ->
    gen_server:call(?MODULE, get_stats).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    State = #state{
        counters = maps:new(),
        start_time = erlang:system_time(second)
    },
    %% Schedule periodic stats reporting
    erlang:send_after(60000, self(), report_stats),
    {ok, State}.

handle_call(get_stats, _From, State) ->
    Uptime = erlang:system_time(second) - State#state.start_time,
    Stats = #{
        counters => State#state.counters,
        uptime_seconds => Uptime,
        memory => erlang:memory(),
        process_count => erlang:system_info(process_count)
    },
    {reply, Stats, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({increment, Counter, Amount}, State) ->
    Counters = State#state.counters,
    Current = maps:get(Counter, Counters, 0),
    NewCounters = maps:put(Counter, Current + Amount, Counters),
    {noreply, State#state{counters = NewCounters}};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(report_stats, State) ->
    %% Log stats
    Uptime = erlang:system_time(second) - State#state.start_time,
    io:format("=== Messaging Service Stats ===~n"),
    io:format("Uptime: ~p seconds~n", [Uptime]),
    io:format("Counters: ~p~n", [State#state.counters]),
    io:format("Processes: ~p~n", [erlang:system_info(process_count)]),
    io:format("Memory: ~p~n", [erlang:memory(total)]),
    
    %% Schedule next report
    erlang:send_after(60000, self(), report_stats),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
