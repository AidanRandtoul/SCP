-module(faultInjector).
-compile(export_all).

-define(Sub_Node, 'sub@').


start(Mode) ->
    register(injector, spawn(faultInjector, injector, [Mode, []])).

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
                    KillerList = spawnKiller(MaxKills, SupList, trunc(5000/NKillers), Mode, NKillers, SupPerKiller);
                Mode == stairs ->
                    timer:sleep(6000),
                    KillerList = [spawn(?MODULE, killer, [MaxKills, lists:sublist(SupList, N, SupPerKiller), Mode]) || N <- lists:seq(1, NSups, SupPerKiller)],
                    io:format("~p:~p~n", [NSups, length(KillerList)]);
                Mode == uniform ->
                    KillerList = spawnKiller(MaxKills, SupList, trunc((1000/MaxKills)/NKillers), Mode, NKillers, SupPerKiller);
                Mode == rand ->
                    KillerList = spawnKiller(MaxKills, SupList, rand:uniform(trunc((1000/MaxKills)/NKillers)), Mode, NKillers, SupPerKiller);
                true ->
                    KillerList = []
            end,
            injector(Mode, [spawn(?MODULE, killer, [Leftover, SupList, Mode]) | KillerList]);

        "Stop" ->
            [exit(Pid, normal) || Pid <- ProcList],
            injector(Mode, [])
    end.

spawnKiller(_, _, _, _, 0, _) -> [];

spawnKiller(NF, SupList, Interval, Mode, N, L) ->
    Pid = spawn(?MODULE, killer, [NF, SupList, Mode]),
    timer:sleep(Interval),
    {HeadList, TailList} = lists:split(L, SupList),
    [Pid | spawnKiller(NF, lists:append(TailList, HeadList), Interval, Mode, N-1, L)].

% Burst fault load specifications
% Single or Multiple Supervisor specification
killer(NF, SupList, burst) ->
    Interval = 5,

    StartTime = erlang:system_time(millisecond),
    NewSupList = burstKill(SupList, NF*Interval),
    Sleep = (Interval * 1000) - (erlang:system_time(millisecond) - StartTime),

    if
        Sleep > 0 ->
            timer:sleep(Sleep);
        true ->
            ok
    end,

    killer(NF, NewSupList, burst);

killer(NF, SupList, stairs) ->
    timer:sleep(5000),
    NewSupList = burstKill(SupList, NF),
    killer(NF, NewSupList, stairs);

% Uniform fault load specification    
killer(NF, [S1 | SupList], uniform) ->
    StartTime = erlang:system_time(millisecond),
    killProcess(S1),
    Sleep = (1000 / NF) - (erlang:system_time(millisecond) - StartTime),
    
    if
        Sleep > 0 ->
            timer:sleep(trunc(Sleep));
        true ->
            ok
    end,
    
    killer(NF, lists:append(SupList, [S1]), uniform);

% Random fault load specification
killer(NF, SupList, rand) ->
    Type = rand:uniform(2),
    Size = rand:uniform(8),

    if
        Type == 1 ->
            NewSupList = randKill(SupList, Size, trunc(1000/NF));
        true ->
            StartTime = erlang:system_time(millisecond),
            NewSupList = burstKill(SupList, Size),
            Sleep = ((1000 / NF) * Size) - (erlang:system_time(millisecond) - StartTime),
            if
                Sleep > 0 ->
                    timer:sleep(trunc(Sleep));
                true ->
                    ok
            end
    end,
    killer(NF, NewSupList, rand).

randKill(SupList, 0, _) -> SupList;

randKill([S1 | SupList], N, Interval) ->
    StartTime = erlang:system_time(millisecond),
    killProcess(S1),
    ElapsedTime = erlang:system_time(millisecond) - StartTime,

    if
        ElapsedTime < Interval ->
            timer:sleep(Interval - ElapsedTime);
        true ->
            ok
    end,
    randKill(lists:append(SupList, [S1]), N-1, Interval).

burstKill(SupList, 0) -> SupList;

burstKill([S1 | SupList], N) ->
    killProcess(S1),
    burstKill(lists:append(SupList, [S1]), N-1).

% Process killer specifications
% Single Supervisor
killProcess(Sup) ->
    PList = supervisor:which_children(Sup),
    {_, Target, _, _} = lists:nth(rand:uniform(length(PList)), PList),
    Status = rpc:call('sub@gpgnode-02', erlang, is_process_alive, [Target]),
    
    if
        Status ->
            exit(Target, kill);
        true ->
            killProcess(Sup)
    end.