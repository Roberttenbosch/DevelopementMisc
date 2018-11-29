-module(dialyzer_test).
-export([test_function/0]).


% test_function() ->
%     X = erlang:time(),
%     seconds(X).


% seconds({_Year, _Month, _Day, Hour, Min, Sec}) ->
%     (Hour * 60 + Min) * 60 + Sec.

% test_function() ->
%     tuple_size(list_to_tuple({a,b,c})).

test_function() -> factorial(-5).

factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).
