-module(request).

-export([start/0,request/3,hash/1]).

-define(LIMIT_CONN, 50).

% contexto = Lista de {Key,Version}

% servidor inicial "tcp://localhost:5555"

start() ->
	Cache = cache:start(),
	Ret = spawn(fun() -> server_manager([],#{},Cache) end),
	connect_server({"localhost",5555},Ret,Cache),
	Ret.

request(add_node,User,_) -> 
	{ok, Sock} = chumak:socket(dealer),

	case chumak:connect(Sock, tcp, "localhost", 5550) of
		{ok, _} -> 
			chumak:send_multipart(Sock,[<<"">>,<<"null|add_node">>]),
			{ok,[_,S]} = chumak:recv_multipart(Sock),
			case S of
				<<"ACK">> -> User ! {resp,"ok"};
				<<"NACK">> -> User ! {resp,"err"}
			end;
		{error, Reason} -> io:format("fail to connect socket: Reason ~p~n",[Reason])
	end;


request({Mode,Dados},User,M) -> 
	case Mode of
		read -> Pid = spawn(fun() -> proc_req(read,length(Dados),length(Dados),M,User) end);
		write -> Pid = spawn(fun() -> proc_req(write,M,User) end)
	end,
	M ! {Mode,Pid,User,Dados}.

proc_req(read,0,Recv,Manager,User,true) -> 
	proc_req(value,{[],[]},Recv,Manager,User);

proc_req(read,0,_,_,_,false) -> ok;

proc_req(read,Waiting,Recv,Manager,User,Flag) ->
	receive
		{version,Key,Version} ->
			case {Flag,Version} of
				{true,"-1"} -> User ! {resp,"err"};
				{true,_} -> 
					Manager ! {value,self(),User,Key,Version},
					proc_req(read,Waiting-1,Recv,Manager,User,Flag);
				{false,_} -> ok
			end;	


		{{redirect,Key,NodeId,Addr},{get_version,Key}} ->
			Manager ! {add_server,Addr,NodeId},
			Manager ! {read,self(),User,[Key],NodeId},
			proc_req(read,Waiting,Recv,Manager,User,Flag)
	end.

proc_req(read,Waiting,Recv,Manager,User) -> proc_req(read,Waiting,Recv,Manager,User,true);

proc_req(value,{Keys,Ctx},0,Manager,User) -> 
	{Correct,Needed,N_needed} = 
	lists:foldr(fun({Key,Version,Valor},{Accum_c,Accum_l,Accum_n}) ->
		case check_ctx({Key,Version},Ctx) of
			{true,_} -> {[{Key,Version,Valor}|Accum_c],Accum_l,Accum_n};
			{false,V} -> {Accum_c,[{Key,V}|Accum_l],Accum_n + 1}
		end
	end,{[],[],0},Keys),

	case N_needed of
		0 ->
			Ret = 
				lists:foldr(fun({Key,Version,Valor},Accum) -> 
					Manager ! {add_ctx,User,[{Key,Version}]},
					Accum ++ "|" ++ Key ++ "|" ++ Valor 
				end,"ok",Keys),	 
			User ! {resp,Ret};
		_ -> 
			io:format("needed -> ~p | correct -> ~p~n",[Correct,Needed]),
			lists:foreach(fun({K,V}) -> Manager ! {value,self(),User,K,V} end,Needed),
			proc_req(value,{Correct,Ctx},N_needed,Manager,User)
	end;

proc_req(value,{Acc_key,Acc_ctx},Num,Manager,User) ->
	receive
		{value,Key,Version,Valor,Ctx} -> 
			Acc_ctx1 = lists:foldr(fun(Elem,Acc) -> insert_ctx(Elem,Acc) end,Acc_ctx,Ctx),
			proc_req(value,{[{Key,Version,Valor}|Acc_key],Acc_ctx1},Num-1,Manager,User);

		{{redirect,Key,NodeId,Addr},{get_key,Key,V}} ->
			Manager ! {add_server,Addr,NodeId},
			Manager ! {read,self(),User,[Key],NodeId},
			proc_req(read,1,1,Manager,self(),false),
			Manager ! {value,self(),User,Key,V},
			proc_req(value,{Acc_key,Acc_ctx},Num,Manager,User)
	end.

proc_req(write,Manager,User) -> 
	receive
		{insert,Key,Version} -> 
			User ! {resp,"ok"},
			Manager ! {update_ctx,User,[{Key,Version}]};

		{{redirect,Key,NodeId,Addr},{write,V}} -> 
			Manager ! {add_server,Addr,NodeId},
			Manager ! {read,self(),User,[Key],NodeId},
			proc_req(read,1,1,Manager,self(),false),
			Manager ! {write,self(),User,[Key,V]},
			proc_req(write,Manager,User)
	end.


% Servers = [{Pid,NodeId}] lista ordenada por NodeId
% Ctx = {User(Pid) -> [{Key,Version}]}
%TODO atutalizar o ctx
server_manager(Servers,Ctx,Cache) ->
	receive
		{add_server,Addr,NodeId} -> 
			Flag = lists:foldr(fun({_,N},Accum) -> (N == str_int(NodeId)) or Accum end,false,Servers), 

			case Flag of
				true -> Servers1 = Servers;
				false -> 
					Pid = connect_server(Addr,NodeId,self(),Cache),
					V = {Pid,str_int(NodeId)},
					case length(Servers) >= ?LIMIT_CONN of
						true -> 
							{Pid_closed,_} = lists:last(Servers),
							Pid_closed ! {close}, 
							Servers1 = insert(V,lists:droplast(Servers));
						false -> Servers1 = insert(V,Servers)
					end 
			end,
			Ctx1 = Ctx;

		{read,Ret,_,Keys} -> 
			lists:foreach(
				fun(Elem) -> 
					server_range(hash(Elem),Servers) ! {get_version,Ret,Elem} 
				end,Keys),
			Ctx1 = Ctx,Servers1 = Servers;

		{read,Ret,_,Keys,NodeId} -> 
			lists:foreach(
				fun(Elem) -> 
					server_pid(str_int(NodeId),Servers) ! {get_version,Ret,Elem} 
				end,Keys),
			Ctx1 = Ctx,Servers1 = Servers;

		{write,Ret,User,[Key,Value]} -> 
			case maps:find(User,Ctx) of
				{ok,Value1} -> User_ctx = Value1;
				error -> User_ctx = [] 
			end,
			Serve_pid = server_range(hash(Key),Servers), 
			Serve_pid ! {write,Ret,Key,Value,User_ctx},
			Ctx1 = Ctx,Servers1 = Servers;

		{value,Ret,_,Key,Version} ->
			server_range(hash(Key),Servers) ! {get_key,Ret,Key,Version},
			Ctx1 = Ctx,Servers1 = Servers;

		{update_ctx,User,Value} -> Ctx1 = maps:put(User,Value,Ctx),Servers1 = Servers;

		{add_ctx,User,Value} -> 
			case maps:find(User,Ctx) of
				{ok,Ctx_user} -> 
					New = lists:foldr(fun(Elem,Acc) -> insert_ctx(Elem,Acc) end,Ctx_user,Value),
					Ctx1 = maps:put(User,New,Ctx);
				error -> 
					New = lists:foldr(fun(Elem,Acc) -> insert_ctx(Elem,Acc) end,[],Value),
					Ctx1 = maps:put(User,New,Ctx)
			end,
			Servers1 = Servers;
		N -> io:format("Nao existe match -> ~p~n",[N]),Servers1 = Servers,Ctx1 = Ctx
	end,
	server_manager(Servers1,Ctx1,Cache).

connect_server({Addr,Port},Manager,_) -> 
	{ok, Sock} = chumak:socket(dealer),

	case chumak:connect(Sock, tcp, Addr, Port) of
		{ok, _} -> 
			chumak:send_multipart(Sock,[<<"">>,<<"null|getLastVersionKey|null">>]),
			{ok,[_,S]} = chumak:recv_multipart(Sock),
			{redirect,_,NodeId,Addr1} = desserialize(S),
			Manager ! {add_server,Addr1,NodeId};
			%Pid = spawn(fun() -> server(Sock,NodeId,{#{},#{},#{}}) end),
			%spawn(fun() -> server_recetor(Sock,Pid) end),
			%Manager ! {add,{Pid,str_int(NodeId)}};
		{error, Reason} -> io:format("fail to connect socket: Reason ~p~n",[Reason])
	end.

connect_server({Addr,Port},NodeId,_,Cache) -> 
	{ok, Sock} = chumak:socket(dealer),
	case chumak:connect(Sock, tcp, Addr, Port) of
		{ok, _} -> 
			Pid = spawn(fun() -> server(Sock,NodeId,{#{},#{},#{}},Cache) end),
			spawn(fun() -> link(Pid),server_recetor(Sock,Pid) end),Pid;
		{error, Reason} -> io:format("fail to connect socket: Reason ~p~n",[Reason])
	end.

% Guardam as mensagens enviadas e a quem tem que responder
% VersionB = #{ Key -> [Pid] }
% KeyB = #{ {Key,version} -> [Pid] }
% WriteB = #{ Key -> [ {Pid,Value} ] }

server(Sock,NodeId,{VersionB,KeyB,WriteB},Cache) -> 
	receive
		{close} ->
			Keys = maps:keys(VersionB) ++ maps:keys(KeyB) ++ maps:keys(WriteB),
			case Keys of
				[] -> 
					exit("closed");
				_ ->
					receive
						A -> self() ! A, self() ! {close}
					end
			end,
			Ret_state = {VersionB,KeyB,WriteB};
		{get_version,Ret,Key} -> 
			case maps:find(Key,VersionB) of
				{ok,Value} -> VersionB1 = maps:put(Key,[Ret|Value],VersionB);
				error -> String = NodeId ++ "|getLastVersionKey|" ++ int_str(Key),
						 chumak:send_multipart(Sock,[<<"">>,list_to_binary(String)]),
						 VersionB1 = maps:put(Key,[Ret],VersionB)
			end,
			Ret_state = {VersionB1,KeyB,WriteB};
			
		{get_key,Ret,Key,Version} -> 
			case maps:find({Key,Version},KeyB) of
				{ok,Value} -> KeyB1 = maps:put({Key,Version},[Ret|Value],KeyB);
				error -> 
					case cache:request(read,Key,Version,Cache) of
						error ->
							String = NodeId ++ "|getKey|" ++ int_str(Key) ++ "|" ++ int_str(Version),
							io:format("~p~n",[String]),
				            chumak:send_multipart(Sock,[<<"">>,list_to_binary(String)]),
				            KeyB1 = maps:put({Key,Version},[Ret],KeyB);
						{V,C} -> Ret ! {value,Key,Version,V,C}, KeyB1 = KeyB
					end 
			end,
			Ret_state = {VersionB,KeyB1,WriteB};
			
		{write,Ret,Key,Value,Ctx} -> 
			case maps:find(Key,WriteB) of
				{ok,Value_dic} -> WriteB1 = maps:put(Key,Value_dic ++ [{Ret,Value}],WriteB);
				error -> WriteB1 = maps:put(Key,[{Ret,Value}],WriteB)
			end,

			String = NodeId ++ "|insertKey|" ++ serialize_version(Key,Value,Ctx), 
			io:format("~p~n",[String]),
			chumak:send_multipart(Sock,[<<"">>,list_to_binary(String)]),
			Ret_state = {VersionB,KeyB,WriteB1};
			
		{recv,{version,Key,Version}} ->
			Users = maps:get(Key,VersionB),
			[Pid ! {version,Key,Version} || Pid <- Users],
			VersionB1 = maps:remove(Key,VersionB),
			Ret_state = {VersionB1,KeyB,WriteB};

		{recv, {value,Key,Version,Value,Ctx}} -> 
			Users = maps:get({Key,Version},KeyB),
			cache:request(write,Key,Version,{Value,Ctx},Cache),
			[Pid ! {value,Key,Version,Value,Ctx} || Pid <- Users],
			KeyB1 = maps:remove({Key,Version},KeyB),
			Ret_state = {VersionB,KeyB1,WriteB};

		{recv,{insert,Key,Version}} -> 
			[{Pid,Value}|T] = maps:get(Key,WriteB),
			cache:request(write,Key,Version,{Value,[{Key,Version}]},Cache),
			Pid ! {insert,Key,Version},
			case T of
				[] -> WriteB1 = maps:remove(Key,WriteB);
				_ -> WriteB1 = maps:put(Key,T,WriteB)
			end,
			Ret_state = {VersionB,KeyB,WriteB1}; 

		{recv,{redirect,Key,NodeId_r,Addr}} -> 
			case maps:find(Key,WriteB) of
				{ok,Value} -> 
					lists:foreach(fun({Pid,V}) -> Pid ! {{redirect,Key,NodeId_r,Addr},{write,V}} end,Value),
					WriteB1 = maps:remove(Key,WriteB);
				_ -> WriteB1 = WriteB
			end,

			case maps:find(Key,VersionB) of 
				{ok,Value1} ->
					[Pid ! {{redirect,Key,NodeId_r,Addr},{get_version,Key}} || Pid <- Value1],
					VersionB1 = maps:remove(Key,VersionB);
				_ -> VersionB1 = VersionB
			end,

			KeyB1 = lists:foldl(fun({Key_list,V},Accum) ->
				case Key == Key_list of
					true -> 
						Users = maps:get({Key,V},Accum),
						[Pid ! {{redirect,Key,NodeId_r,Addr},{get_key,Key,V}} || Pid <- Users],
						maps:remove({Key,V},Accum);
					false -> Accum 
				end end,KeyB,maps:keys(KeyB)),

			Ret_state = {VersionB1,KeyB1,WriteB1};
		A -> io:format("mensagem n conhecida Server ~p~n",[A]),Ret_state ={VersionB,KeyB,WriteB}

	end,
	server(Sock,NodeId,Ret_state,Cache).

server_recetor(Sock,Pid) ->
	{ok,[_,Data]} = chumak:recv_multipart(Sock),
	Parsed_data = desserialize(Data),
	Pid ! {recv,Parsed_data},
	server_recetor(Sock,Pid).

insert_ctx({K,V},[{K1,V1}|T]) ->
	V_int = str_int(V),
	V1_int = str_int(V1),
	case K == K1 of
		true when V_int > V1_int -> [{K,V}|T];
		true -> [{K,V1}|T];
		false -> [{K1,V1}|insert_ctx({K,V},T)]
	end;

insert_ctx(K,[]) -> [K].

check_ctx({K,V},[{K1,V1}|T]) ->
	case K == K1 of
		true -> {str_int(V) >= str_int(V1),V1};
		false -> check_ctx({K,V},T)
	end;

check_ctx(_,[]) -> {true,[]}.

insert({Pid,Id},[{P,X}|T]) ->
	case Id > X of
		true -> [{P,X}|insert({Pid,Id},T)];
		false -> [{Pid,Id},{P,X}|T] 
	end;

insert(A,[]) -> [A].

server_range(Elem,[{Pid,N}|T]) -> 
	case Elem < N of
		true -> Pid;
		false -> server_range(Elem,T,Pid)
	end.

server_range(Elem,[{Pid,N}|T],H) -> 
	case Elem < N of
		true -> Pid;
		false -> server_range(Elem,T,H)
	end;

server_range(_,[],H) -> H.

server_pid(_,[{Pid,_}]) -> Pid;
server_pid(NodeId,[{Pid,NodeId}|_]) -> Pid;
server_pid(NodeId,[_|T]) ->server_pid(NodeId,T).


% Recebida mensagem
% redirect -> {redirect,key sended,NodeId,{Host,Port}}
% get version -> {version,key,Versao}
% get valor -> {value,Key,Version,Valor,Ctx} Ctx = [{Key,Version}]
% insert -> {insert,Key,Version}

desserialize(S) -> 
	String = binary:bin_to_list(S),
	Parsed_string = lists:foldr(fun(Elem,Acc) -> split(Elem,Acc) end,[],String),
	case Parsed_string of
		[Key,"successor",NodeId,Addr] -> {redirect,Key,NodeId,parse_addr(Addr)};
		[Key,Version] -> {version,Key,Version};
		[Key,Version,"inserted"] -> {insert,Key,Version};
		[Key,Version,"empty",Value] -> {value,Key,Version,Value,[]};
		[Key,Version|T] -> {value,Key,Version,lists:last(T),parse_ctx(T)}
	end.

serialize_version(Key,Value,[]) ->
	Key ++ "|" ++ "empty|" ++ Value;

serialize_version(Key,Value,Ctx) ->
	Key ++ "|" ++ serialize_version(Ctx) ++ Value.

serialize_version([{Key,Version}|T]) ->
	Key ++ "|" ++ Version ++ "|" ++ serialize_version(T);

serialize_version([]) -> "".

int_str(S) -> [N] = io_lib:fwrite("~p",[S]),lists:filter(fun(Elem) -> Elem /= $" end,N).

str_int(S) -> {R,_} = string:to_integer(S),R.

parse_ctx([K,V|T]) -> [{K,V}|parse_ctx(T)];
parse_ctx([_]) -> [].

parse_addr("tcp://"++Host) -> 
	[A,B] = string:split(Host,":"),
	{A,str_int(B)}.

split($\n,A) -> A;
split(0,A) -> A;    
split($|,[[]|T]) -> [[]|T];
split($|,A) -> [[]] ++ A;
split(Elem,[H|T]) -> [[Elem] ++ H|T];
split(Elem,[]) -> [[Elem]].

hash(Key) -> 
	Digest = crypto:hash(sha224,Key),
	Int = binary:part(Digest,{0,4}),
	L = binary_to_list(Int),
	Ret = lists:foldl(fun(Elem,Acc) -> ((Acc bsl 8) bor (Elem band 16#FF)) band 16#7FFFFFFF end,0 ,L),
	Ret.