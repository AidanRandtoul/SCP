-module(run).
-export([bigrun/0, run/2, startRun/4]).

bigrun() ->
    Ratios = [{1024,1}, {512,2}, {256,4}, {128,8}, {64,16}, {32,32}, {16,64}, {8,128}, {4,256}, {2,512}, {1,1024}],
    io:format("Beginning big test~n"),
    [run(P, S) || {P,S} <- Ratios].

run(NPairs, NSups) ->
    Values = [0, 1, 2.5, 5, 10, 20],
    Strat = {one_for_one, 200000, 1},
    {ok, Fd} = file:open("result.txt", [append]),
    io:format(Fd, "Test started! Running ~p:~p ratio~n", [NPairs, NSups]),
    [startRun(NPairs, NSups, V, Strat) || V <- Values],
    io:format(Fd, "Test complete!~n", []).

startRun(P, S, F, Strat) ->
    sub ! {"Start", P, S, F, Strat},
    timer:sleep(70*1000),
    sub ! "Stop",
    timer:sleep(15*1000).