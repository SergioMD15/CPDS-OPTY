-module(entry).
-export([new/1]).

new(Value) ->
    spawn_link(fun() -> init(Value) end).

init(Value) ->
    entry(Value, make_ref(), []).

entry(Value, Time, ActiveReaders) ->
    receive
        {read, Ref, From, TransactionId} ->
            From ! {Ref, self(), Value, Time},
            case lists:member(TransactionId, ActiveReaders) of
                true ->
                    entry(Value, Time, ActiveReaders);
                false -> 
                    entry(Value, Time, [TransactionId | ActiveReaders])

            end;
        {write, New} ->
            entry(New, make_ref(), ActiveReaders);
        {check, Ref, From} ->
            case ActiveReaders of
                [] -> From ! {Ref, ok};
                _ -> From ! {Ref, abort}
            end,
            entry(Value, Time, ActiveReaders);
        {deleteReads, TransactionId, Ref, From} ->
            From ! Ref,
            entry(Value, Time, [ActiveReaderId || ActiveReaderId <- ActiveReaders, ActiveReaderId /= TransactionId]);
        stop ->
            ok
    end.
