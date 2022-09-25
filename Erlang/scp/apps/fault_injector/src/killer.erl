-module(killer).

-export([spawn_killer/6]).

-export([killer/3, burst_kill/2, rand_kill/3, kill_process/1]).



spawn_killer(_, _, _, _, 0, _) -> [];

spawn_killer(NF, SupList, Interval, Mode, N, L) ->
    Pid = spawn(?MODULE, killer, [NF, SupList, Mode]),
    timer:sleep(Interval),
    {HeadList, TailList} = lists:split(L, SupList),
    [Pid | spawn_killer(NF, lists:append(TailList, HeadList), Interval, Mode, N-1, L)].

% Burst fault load specifications
% Single or Multiple Supervisor specification
killer(NF, SupList, burst) ->
    Interval = 5,

    StartTime = erlang:system_time(millisecond),
    NewSupList = burst_kill(SupList, NF*Interval),
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
    NewSupList = burst_kill(SupList, NF),
    killer(NF, NewSupList, stairs);

% Uniform fault load specification    
killer(NF, [S1 | SupList], uniform) ->
    StartTime = erlang:system_time(millisecond),
    kill_process(S1),
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
            NewSupList = rand_kill(SupList, Size, trunc(1000/NF));
        true ->
            StartTime = erlang:system_time(millisecond),
            NewSupList = burst_kill(SupList, Size),
            Sleep = ((1000 / NF) * Size) - (erlang:system_time(millisecond) - StartTime),
            if
                Sleep > 0 ->
                    timer:sleep(trunc(Sleep));
                true ->
                    ok
            end
    end,
    killer(NF, NewSupList, rand).

rand_kill(SupList, 0, _) -> SupList;

rand_kill([S1 | SupList], N, Interval) ->
    StartTime = erlang:system_time(millisecond),
    kill_process(S1),
    ElapsedTime = erlang:system_time(millisecond) - StartTime,

    if
        ElapsedTime < Interval ->
            timer:sleep(Interval - ElapsedTime);
        true ->
            ok
    end,
    rand_kill(lists:append(SupList, [S1]), N-1, Interval).

burst_kill(SupList, 0) -> SupList;

burst_kill([S1 | SupList], N) ->
    kill_process(S1),
    burst_kill(lists:append(SupList, [S1]), N-1).

% Process killer specifications
% Single Supervisor
kill_process(Sup) ->
    PList = supervisor:which_children(Sup),
    {_, Target, _, _} = lists:nth(rand:uniform(length(PList)), PList),
    Status = rpc:call('<SubVM>@<SubNode>', erlang, is_process_alive, [Target]),
    
    if
        Status ->
            exit(Target, kill);
        true ->
            kill_process(Sup)
    end.