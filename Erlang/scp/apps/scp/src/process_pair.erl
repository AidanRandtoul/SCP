-module(process_pair).

-export([spawn_server/1, spawn_client/0]).

-export([server/3, client/0]).


spawn_client() ->
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

spawn_server(Data) ->
    Pid = spawn_link(?MODULE, server, [spawn_link(?MODULE, client, []), 0, Data]),
    Pid ! "Start",
    {ok, Pid}.

server(Clients, MessageCount, Data) ->
    receive
        "Start" ->
            Clients ! {"Hello",self(), Data},
            server(Clients, MessageCount, Data);
        {"Done", Pid, Data} ->
            counter ! 1,
            Pid ! {"Hello",self(), Data},
            server(Clients, MessageCount + 1, Data);
        "Stop" ->
            exit(kill);
        _ ->
            io:format("Unexpected message received!~n")
    end.