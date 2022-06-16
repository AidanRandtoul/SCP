-module(processSpec).
-export([spawnClient/0, spawnServer/2, spawnCounter/0, spawnLogger/1]).
-export([client/0, server/4, aggregator/3, logger/1]).

spawnClient() ->
    {ok, spawn_link(?MODULE, client, [])}.

client() ->
    receive
        {"Hello", Pid, Data} ->
            timer:sleep(200),
            Pid ! {"Done", self(), Data},
            client();
        _ ->
            io:format("Unexpected message received!~n")
    end.

spawnServer(Counter, Data) ->
    Pid = spawn_link(?MODULE, server, [Counter, spawn_link(?MODULE, client, []), 0, Data]),
    Pid ! "Start",
    {ok, Pid}.

server(Counter, Clients, MessageCount, Data) ->
    receive
        "Start" ->
            Clients ! {"Hello",self(), Data},
            server(Counter, Clients, MessageCount, Data);
        {"Done", Pid, Data} ->
            Counter ! 1,
            Pid ! {"Hello",self(), Data},
            server(Counter, Clients, MessageCount + 1, Data);
        "Stop" ->
            exit(kill);
        _ ->
            io:format("Unexpected message received!~n")
    end.

spawnCounter() ->
    Pid = spawn_link(?MODULE, aggregator, [0, erlang:system_time(millisecond), []]),
    register(counter, Pid),
    spawn_link(?MODULE, logger, [Pid]),
    {ok, Pid}.

aggregator(N, StartTime, Record) ->
    receive
        "Start" ->
            aggregator(0, erlang:system_time(millisecond), []);
        "Update" ->
            ElapsedTime = (erlang:system_time(millisecond) - StartTime)/1000,
            Throughput = N/ElapsedTime,
            aggregator(0, erlang:system_time(millisecond), [Throughput | Record]);
        "Stop" ->
            Avg = lists:sum(Record) / length(Record),
            {ok, Fd} = file:open("result.txt", [append]), 
            io:format(Fd, "~.3f~n", [Avg]);
            %[io:format(Fd, "~.2f~n", [D]) || D <- lists:reverse(Record)];
        MessagesCount ->
            aggregator(N+MessagesCount, StartTime, Record)
    end.

spawnLogger(Counter) ->
    {ok, spawn(?MODULE, logger, [Counter])}.

logger(Counter) ->
    timer:sleep(1000),
    Counter ! "Update",
    logger(Counter).