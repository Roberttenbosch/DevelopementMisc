-module(dynamic_loading_a).
-compile(export_all).

start(Tag) ->
    spawn(fun() -> loop(Tag) end).


loop(Tag) ->
    sleep(),
    Val = dynamic_loading_b:x(),
    io:format("Vs3 (~p) b:x() = ~p~n", [Tag, Val]),
    loop(Tag).

sleep() ->
    receive
        after 3000 -> true
    end.
