-module(handler).
-export([start/3]).

start(Client, Validator, Store) ->
    spawn_link(fun() -> init(Client, Validator, Store) end).

init(Client, Validator, Store) ->
    TransactionId = make_ref(),
    handler(Client, Validator, Store, [], [], TransactionId).

handler(Client, Validator, Store, Reads, Writes, TransactionId) ->         
    receive
        {read, Ref, N} ->
            case lists:keyfind(N, 1, Writes) of
                {N, _, Value} ->
                    Client ! {value, Ref, Value},
                    handler(Client, Validator, Store, Reads, Writes, TransactionId);
                false ->
                    store:lookup(N, Store) ! {read, Ref, self(), TransactionId},
                    handler(Client, Validator, Store, Reads, Writes, TransactionId)
            end;
        {Ref, Entry, Value, Time} ->
            Client ! {value, Ref, Value},
            handler(Client, Validator, Store, [{Entry, Time}|Reads], Writes, TransactionId);
        {write, N, Value} ->
            Added = lists:keystore(N, 1, Writes, {N, store:lookup(N, Store), Value}),
            handler(Client, Validator, Store, Reads, Added, TransactionId);
        {commit, Ref} ->
            Validator ! {validate, Ref, Reads, Writes, Client, TransactionId};
        abort ->
            ok
    end.
