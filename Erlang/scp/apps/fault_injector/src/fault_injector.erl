-module(fault_injector).

-export([start/1, stop/0]).

-export([injector/2]).

-import(killer, [spawn_killer/6]).

start(Mode) ->
    register(injector, spawn(fault_injector, injector, [Mode, []])).

stop() -> 
    exit(whereis(injector), finish).

injector(Mode, ProcList) ->
    receive
        {"Start", SupList, NFailures} ->
            io:format("Generating faultload: ~p per second, Mode: ~p~n", [NFailures, Mode]),

            NSups = length(SupList),                         % Number of Supervisors in the system
            MaxKills = 5,                                    % Max number of times a killer kills per second
            NKillers = NFailures div MaxKills,               % Number of killers required for current fault load
            Leftover = NFailures rem MaxKills,               % Extra killer needed if NFailures is not perfectly divisible by MaxKills
            
            if
                NSups > NKillers ->
                    if
                        NSups rem NKillers /= 0 ->
                            SupPerKiller = (NSups div NKillers) + 1;
                        true ->
                            SupPerKiller = NSups div NKillers
                    end;
                true ->
                    SupPerKiller = 1
            end,

            if 
                Mode == burst ->
                    KillerList = spawn_killer(MaxKills, SupList, trunc(5000/NKillers), Mode, NKillers, SupPerKiller);
                Mode == stairs ->
                    timer:sleep(6000),
                    KillerList = [spawn(?MODULE, killer, [MaxKills, lists:sublist(SupList, N, SupPerKiller), Mode]) || N <- lists:seq(1, NSups, SupPerKiller)],
                    io:format("~p:~p~n", [NSups, length(KillerList)]);
                Mode == uniform ->
                    KillerList = spawn_killer(MaxKills, SupList, trunc((1000/MaxKills)/NKillers), Mode, NKillers, SupPerKiller);
                Mode == rand ->
                    KillerList = spawn_killer(MaxKills, SupList, rand:uniform(trunc((1000/MaxKills)/NKillers)), Mode, NKillers, SupPerKiller);
                true ->
                    KillerList = []
            end,
            injector(Mode, [spawn(?MODULE, killer, [Leftover, SupList, Mode]) | KillerList]);

        "Stop" ->
            [exit(Pid, normal) || Pid <- ProcList],
            injector(Mode, [])
    end.