-module(scp).

-export([start/0, benchmark/2, start_system/4]).

-export([counter_spec/0]).

start() ->
    register(sub, spawn(?MODULE, benchmark, ['<InjectorVM>@<InjectorNode>', 0])).

benchmark(FNode, SystemPid) ->
    receive
        {"Start", NPairs, NSups, NFails} ->
            Pid = start_system(NPairs, NSups, NFails, FNode),
            benchmark(FNode, Pid);
        "Stop" ->
            counter ! "Stop",
            timer:sleep(1000),
            {injector, FNode} ! "Stop",
            exit(SystemPid, normal),
            io:format("Benchmark finished!~n"),
            benchmark(FNode, 0);
        _ ->
            io:format("Unknown command received!~n")
    end.

start_system(NumPairs, NumSups, FailureRate, InjectorNode) ->
    TotalPairs = NumPairs*NumSups,

    StartTime = erlang:system_time(millisecond),

    % Create top level supervisor
    {ok, HeadSupPid} = head_sup:start_link(NumPairs, NumSups),
    SupList = [ SupPid || {_, SupPid, _, _} <- supervisor:which_children(HeadSupPid)],

    head_supervisor:start_child(HeadSupPid, counter_spec()),

    ElapsedTime = erlang:system_time(millisecond) - StartTime,
    io:format("~p Process Pairs spawned and started in ~p seconds.~n", [TotalPairs, ElapsedTime/1000]),
    io:format("Starting benchmark!~n"),

    if
        FailureRate =/= 0 ->
            {injector, InjectorNode} ! {"Start", SupList, trunc(TotalPairs*5*(FailureRate))},
            timer:sleep(5000);
        true -> ok   
    end,
    counter ! "Start",
    HeadSupPid.  


% internal functions

counter_spec() -> 
    #{id => counter,
    start => {counter, spawn_counter, []},
    restart => transient,
    type => worker,
    modules => [counter]}.