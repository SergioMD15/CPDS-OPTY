-module(opty).
-export([start/5, stop/2]).

%% Clients: Number of concurrent clients in the system
%% Entries: Number of entries in the store
%% Reads: Number of read operations per transaction
%% Writes: Number of write operations per transaction
%% Time: Duration of the experiment (in secs)

start(Clients, Entries, Updates, Time, Server) ->
    L = startClients(Clients, [], Entries, Updates,Server),
    io:format("Starting: ~w CLIENTS, ~w ENTRIES, ~w Updates per transaction, DURATION ~w s~n",
         [Clients, Entries, Updates, Time]),
    timer:sleep(Time*1000),
    stop(L,Server).

stop(L,Server) ->
    io:format("Stopping...~n"),
    stopClients(L),
    Server ! stop.

startClients(0, L, _, _, _) -> L;
startClients(Clients, L, Entries, Updates,Server) ->
    Pid = client:start(Clients, Entries, Updates,Server),
    startClients(Clients-1, [Pid|L], Entries, Updates,Server).

stopClients([]) ->
    ok;
stopClients([Pid|L]) ->
    Pid ! {stop, self()},
    receive
        {done, Pid} -> ok
    end,
    stopClients(L).
