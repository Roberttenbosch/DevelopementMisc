-module(try_test).
-export([demo/0, demo2/0]).


generate_exception(1) -> a;
generate_exception(2) -> throw(a);
generate_exception(3) -> exit(a);
generate_exception(4) -> {'EXIT', a};
generate_exception(5) -> error(a).


demo() ->
    [catcher(I) || I <- [1,2,3,4,5]].

demo2() ->
    [catch generate_exception(I) || I <- [1,2,3,4,5]].


catcher(N) ->
    try generate_exception(N) of
        Val -> {N, normal, Val}
    catch
        throw:X -> {N, caught, thrown, X};
        exit:X -> {N, caught, exit, X};
        error:X -> {N, caught, error, X}
     end.

