-module(benchmark).
-compile(export_all).

start() ->
    register(sub, spawn(?MODULE, benchmark, ['fault@gpgnode-01', 0])).

benchmark(FNode, SystemPid) ->
    receive
        {"Start", NPairs, NSups, NFails, SupStrat} ->
            Pid = startSystem(NPairs, NSups, NFails, SupStrat, FNode),
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

startSystem(NumPairs, NumSupervisors, NFailures, SupStrat, FNode) ->
    TotalPairs = NumPairs*NumSupervisors,
    StartTime = erlang:system_time(millisecond),
    {ok, HeadSupName} = processSup:start_link({headSupervisor, {SupStrat, [genChildSpec(counter, spawnCounter, [])]}}),
    [{_, Counter, _, _}] = supervisor:which_children(HeadSupName),
    Children = [genChildSpec(genName("server", N), spawnServer, [Counter, bytes_generate(500)]) || N <- lists:seq(1, NumPairs)],
    SupervisorList = [spawnSupervisor(HeadSupName, SupStrat, Children, N) || N <- lists:seq(1, NumSupervisors)],
    ElapsedTime = erlang:system_time(millisecond) - StartTime,
    io:format("~p Process Pairs spawned and started in ~p seconds.~n", [TotalPairs, ElapsedTime/1000]),
    io:format("Starting benchmark!~n"),
    if
        NFailures =/= 0 ->
            {injector, FNode} ! {"Start", SupervisorList, trunc(TotalPairs*5*(NFailures/100))},
            timer:sleep(5000);
        true -> ok   
    end,
    Counter ! "Start",
    HeadSupName.  

spawnSupervisor(HeadSup, SupStrategy, ChildList, SupervisorNum) ->
    NewName = genName("subSupervisor", SupervisorNum),
    {ok, NewSup} = supervisor:start_child(HeadSup, genSupervisorSpec(NewName, [{NewName, {SupStrategy, ChildList}}])),
    NewSup.

genSupervisorSpec(Name, Args) ->
    #{id => Name, 
    start => {processSup, start_link, Args},
    restart => transient, 
    type => supervisor, 
    modules => [processSup]}.

genChildSpec(Name, Spawner, Args) ->
    #{id => Name,
    start => {processSpec, Spawner, Args},
    restart => transient,
    type => worker,
    modules => [processSpec]}.

bytes_generate(Size) ->
    bytes_generate(Size, []).

bytes_generate(0, Bytes) ->
    list_to_binary(Bytes);

bytes_generate(Size, Bytes) ->
    bytes_generate(Size - 1, [1 | Bytes]).

genName(Type, N) ->
    list_to_atom(string:concat(Type, integer_to_list(N))).