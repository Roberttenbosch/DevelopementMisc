-module(udp_test).
-export([start_server/0, client/1,start_client/1]).


start_server() ->
    spawn(fun() -> server(4000)end).

server(Port) ->
    {ok, Socket} = gen_udp:open(Port, [binary]),
    io:format("Server opened socket:~p~n", [Socket]),
    loop(Socket).

loop(Socket) ->
    receive
        {udp, Socket, Host, Port, Bin} = Msg ->
            io:format("server received:~p~n", [Msg]),
            {Ref, N} = binary_to_term(Bin),
            io:format("Ref: ~p || N: ~p~n",[Ref,N]),
            Fac = fac(N),
            gen_udp:send(Socket, Host, Port, term_to_binary({Ref, Fac})),
            loop(Socket)
    end.


fac(0) -> 1;
fac(N) when (N > 0) -> N * fac(N - 1).

start_client(N) ->
    spawn(fun() -> client(N)end).
  
client(N) ->
    {ok, Socket} = gen_udp:open(0, [binary]),
    io:format("client openend socket: ~p~n", [Socket]),
    Ref = make_ref(),
    B1 = term_to_binary({Ref, N}),
    ok = gen_udp:send(Socket, "localhost", 4000,B1),
    wait_for_ref(Socket, Ref).

wait_for_ref(Socket, Ref) ->
    receive
        {udp, Socket, _, _, Bin} = Msg ->
            io:format("client received: ~p~n", [Msg]),
            case binary_to_term(Bin) of
                {Ref, Val} ->
                    io:format("value: ~p~n",[Val]);
                {_OtherRef, _} ->
                    wait_for_ref(Socket, Ref)
            end,
            binary_to_term(Bin)
    after 2000 ->
        io:format("timeout~n")
    end,
    gen_udp:close(Socket).

