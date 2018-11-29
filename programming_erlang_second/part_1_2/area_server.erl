-module(area_server).
-export([start/0, loop/0, area/2]).

start() -> spawn(area_server, loop, []).
area(Pid, What) ->
    rpc(Pid, What).

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} ->
            Response
    end.

loop() ->
    receive
        {From, {rectangle, Width, Ht}} ->
            From ! {self(), Width * Ht},
            loop();
        {From, {square, Side}} ->
            From ! {self(), Side * Side},
            loop();
        {From , Other} ->
            From ! {self(), {error, undefined_pattern ,Other}}
    end.
