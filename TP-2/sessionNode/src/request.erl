-module(request).

-export([start/0,request/1]).

% contexto = Lista de {Key,Version}

% servidor inicial "tcp://localhost:5555"

start() ->
	register(?MODULE,spawn(fun() -> server_manager([]) end)),
	connect_server("localhost",5555).

request(R) -> ok.

% Servers = [{Pid,NodeId}] lista ordenada por NodeId
server_manager(Servers) ->
	receive
		{add,V} -> Servers1 = insert(V,Servers)
	end,
	server_manager(Servers).

connect_server(Addr,Port) -> 
	{ok, Sock} = chumak:socket(dealer,self()),

	case chumak:connect(Sock, tcp, Addr, Port) of
		{ok, BindPid} -> 
			chumak:send_multipart(Sock,[<<"">>,<<"getLastKeyVersion">>,<<"ola">>]),
			{ok,Multipart} = chumak:recv_multipart(Sock),
			io:format("First request Multipart: ~p~n",[Multipart]),
			spawn(fun() -> serve_recetor(Sock,self()) end),
			?MODULE ! {add,{self(),2}},
			server(Sock,"ola",#{},#{},#{}); 
		{error, Reason} -> io:format("fail to connect socket: Reason ~p~n",[Reason])
	end.

connect_server(Addr,Port,NodeId) -> 
	{ok, Sock} = chumak:socket(dealer,self()),

	case chumak:connect(Sock, tcp, Addr, Port) of
		{ok, BindPid} -> 
			spawn(fun() -> serve_recetor(Sock,self()) end),
			server(Sock,NodeId,#{},#{},#{}); 
		{error, Reason} -> io:format("fail to connect socket: Reason ~p~n",[Reason])
	end.

server(Sock,NodeId,Version,Key,Write) -> 
	receive
		{get_version,Key} -> chumak:send_multipart(Sock,[<<"">>,<<"getLastKeyVersion">>,<<Key>>]);
		{get_key,Key,Version} -> chumak:send_multipart(Sock,[<<"">>,<<"getKey">>,<<Key>>,<<Version>>]);
		{write,Key,Value,Ctx} -> chumak:send_multipart(Sock,[<<"">>,<<"insertKey">>,<<Value>>])
	end.

serve_recetor(Sock,Pid) ->
	{ok,Multipart} = chumak:recv_multipart(Sock),
	io:format("Multipart: ~p~n",[Multipart]),
	Pid ! Multipart,
	serve_recetor(Sock,Pid).




insert({Pid,Id},[{P,X}|T]) ->
	case {Id > X} of
		true -> [{P,X}|insert({Pid,Id},T)];
		false -> [{Pid,Id},{P,X}|T] 
	end;

insert(A,[]) -> [A].