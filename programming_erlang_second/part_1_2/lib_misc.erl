-module(lib_misc).
-export([keep_alive/2, my_little_function/0, display_why/2, on_exit/2, pythag/1, for/3, sum/1, map/2, qsort/1, odds_and_evens/1, count_chars/1, my_sleep/1, flush_buffer/0, priority_receive/0]).
-import(lists, [reverse/1]).

for(Max, Max, F) -> [F(Max)];
for(I, Max, F) -> [F(I) | for(I+1, Max, F)].

sum([H|T]) -> H + sum(T);
sum([]) -> 0.

map(_, []) -> [];
map(F, [H|T]) -> [F(H)| map(F, T)].

qsort([]) -> [];
qsort([Pivot | T]) ->
    qsort([X || X <- T, X < Pivot])
    ++ [Pivot] +++
    qsort([X || X <- T, X >= Pivot]).


pythag(N) ->
    [ {A, B, C} ||
        A <- lists:seq(1, N),
        B <- lists:seq(1, N),
        C <- lists:seq(1, N),
        A + B + C =< N,
        A*A + B*B =:= C*C
    ].

odds_and_evens(L) ->
    odds_and_evens_acc(L, [], []).

odds_and_evens_acc([H|T], Odds, Evens) ->
    case (H rem 2) of
        1 -> odds_and_evens_acc(T, [H|Odds], Evens);
        0 -> odds_and_evens_acc(T, Odds, [H|Evens])
    end;
odds_and_evens_acc([], Odds, Evens) ->
    {{odds, reverse(Odds)}, {evens,reverse(Evens)}}.


count_chars(Str) ->
    count_chars(Str, #{}).

count_chars([H|T], X) ->
    case X of
        #{H := N} ->
            count_chars(T, X#{H := N + 1});
        _ ->
        count_chars(T, X#{H => 1})
    end;
count_chars([], X) ->
    X.


my_sleep(Time) ->
    receive
    after Time ->
        true
    end.


flush_buffer() ->
    receive
        _Any -> 
            flush_buffer()
    after 0 ->
        true
    end.


priority_receive() ->
    receive 
        {alarm, X} ->
            {alarm, X}
    after 0 -> 
        receive
            Any ->
                Any
        end
    end.

on_exit(Pid, Fun) ->
    spawn(fun() -> 
            Ref = monitor(process, Pid),
            receive
                {'DOWN', Ref, process, Pid, Why} ->
                    Fun(Pid, Why)
                end
            end).

display_why(Pid, Why) ->
    io:format("Pid ~p died with reason ~p~n", [Pid, Why]).

my_little_function() ->
    receive 
        X -> Y = list_to_atom(X),
            io:format("Y = ~p", [Y])
    end.


keep_alive(Name, Fun) ->
    register(Name, Pid = spawn(Fun)),
    on_exit(Pid, fun(_Why) -> keep_alive(Name, Fun) end).