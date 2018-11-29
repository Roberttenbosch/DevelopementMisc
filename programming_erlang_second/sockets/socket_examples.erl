-module(socket_examples).

-export([nano_client_eval_fixed/0,start_parallel_server/0,nano_get_url/0, start_nano_server/0,nano_client_eval/1,start_seq_server/0]).

-import(lists, [reverse/1]).
-import(lib_misc, [string2value/1]).

nano_get_url() -> nano_get_url("www.google.com").

start_nano_server() ->
    {ok, Listen} = gen_tcp:listen(2345,
				  [binary, {packet, 4}, {reuseaddr, true},
				   {active, true}]),
    {ok, Socket} = gen_tcp:accept(Listen),
    gen_tcp:close(Listen),
    loop(Socket).

start_seq_server() ->
    {ok, Listen} = gen_tcp:listen(2345,
				  [binary, {packet, 4}, {reuseaddr, true},
				   {active, true}]),
    seq_loop(Listen).

seq_loop(Listen) ->
   {ok, Socket} = gen_tcp:accept(Listen),
    loop(Socket),
    seq_loop(Listen).

start_parallel_server() ->
    {ok, Listen} = gen_tcp:listen(2345,
				  [binary, {packet, 4}, {reuseaddr, true},
				   {active, true}]),
    spawn(fun() -> par_connect(Listen) end).

par_connect(Listen) ->
      {ok, Socket} = gen_tcp:accept(Listen),
      spawn(fun() -> par_connect(Listen) end),
      loop(Socket).

loop(Socket) ->
    receive
      {tcp, Socket, Bin} ->
	  io:format("Server received binary = ~p~n", [Bin]),
	  Str = binary_to_term(Bin),
	  io:format("Server unpacked ~p ~n", [Str]),
	  Reply = lib_misc:string2value(Str),
	  io:format("Server replying = ~p~n", [Reply]),
	  gen_tcp:send(Socket, term_to_binary(Reply)),
	  loop(Socket);
      {tcp_closed, Socket} ->
	  io:format("Server socket ~p closed~n", [Socket])
    end.

nano_client_eval_fixed() ->
    nano_client_eval("list_to_tuple([2 + 3 * 4, 10+20])").

nano_client_eval(Str) ->
    {ok, Socket} = gen_tcp:connect("localhost", 2345,
				   [binary, {packet, 4}]),
    ok = gen_tcp:send(Socket, term_to_binary(Str)),
    receive
      {tcp, Socket, Bin} ->
	  io:format("Client received binary = ~p~n", [Bin]),
	  Val = binary_to_term(Bin),
	  io:format("Client result = ~p~n", [Val]),
	  gen_tcp:close(Socket)
    end.

nano_get_url(Host) ->
    {ok, Socket} = gen_tcp:connect(Host, 80,
				   [binary, {packet, 0}]),
    ok = gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"),
    receive_data(Socket, []).

receive_data(Socket, SoFar) ->
    receive
      {tcp, Socket, Bin} ->
	  receive_data(Socket, [Bin | SoFar]);
      {tcp_closed, Socket} -> list_to_binary(reverse(SoFar))
    end.
