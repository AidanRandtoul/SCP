-module(scp_counter).

-export([spawn_counter/0]).

-export([counter/3, logger/1]).


spawn_counter() ->
    Pid = spawn_link(?MODULE, counter, [0, erlang:system_time(millisecond), []]),
    register(counter, Pid),
    spawn_link(?MODULE, logger, [Pid]),
    {ok, Pid}.


counter(N, StartTime, Record) ->
    receive
        "Start" ->
            counter(0, erlang:system_time(millisecond), []);
        "Update" ->
            ElapsedTime = (erlang:system_time(millisecond) - StartTime)/1000,
            Throughput = N/ElapsedTime,
            counter(0, erlang:system_time(millisecond), [Throughput | Record]);
        "Stop" ->
            Avg = lists:sum(Record) / length(Record),
            {ok, Fd} = file:open("result.txt", [append]), 
            io:format(Fd, "~.3f~n", [Avg]);
            %[io:format(Fd, "~.2f~n", [D]) || D <- lists:reverse(Record)];
        MessagesCount ->
            counter(N+MessagesCount, StartTime, Record)
    end.


logger(Counter) ->
    timer:sleep(1000),
    Counter ! "Update",
    logger(Counter).