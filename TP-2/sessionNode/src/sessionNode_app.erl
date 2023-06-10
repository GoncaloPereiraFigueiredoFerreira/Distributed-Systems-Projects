%%%-------------------------------------------------------------------
%% @doc sessionNode public API
%% @end
%%%-------------------------------------------------------------------

-module(sessionNode_app).

-behaviour(application).

-export([start/2, stop/1, main/1]).

-define(TIME, 60).
-define(UPDATE, 1).
-define(LIMIT, 1000).
-define(MAX_MESSAGE, 100).


start(_StartType, _StartArgs) ->
    sessionNode_sup:start_link(),
    sessionManager:start(),
    spawn(fun() -> main(12345) end),
    spawn(fun() -> main(12346) end),
    main(12347).

stop(_State) ->
    ok.

main(Port) ->
    io:format("Sessao na porta ~p~n",[Port]),
    Request = request:start(),
    {ok, LSock} = gen_tcp:listen(Port, [{active, once}, {packet, line},{reuseaddr, true}]),
    Session = spawn(fun() -> session({#{},orSet:new(),orSet:new(),[]},Request) end),
    sessionManager:add_session(Session),
    spawn(fun() -> acceptor(LSock, Session) end),
    timer(?UPDATE,Session),
    ok.

timer(Time,Pid) ->
    receive
        after round(Time * 1000) ->
            Pid ! {timer},
            timer(Time,Pid)
    end.

acceptor(LSock, Session) ->
      {ok, Sock} = gen_tcp:accept(LSock),
      spawn(fun() -> acceptor(LSock, Session) end),
      Login = login(Sock),
      Session ! Login,
      user(Sock,Session).

login(Sock) ->
    receive
        {tcp,_,Data} ->
            inet:setopts(Sock, [{active, once}]),
            case proc_data(parse_data(Data)) of
                {login,Pid,Name} -> gen_tcp:send(Sock, "ok"),{login,Pid,Name};
                _ -> gen_tcp:send(Sock, "ok"),login(Sock)
            end
    end.

% Estado interno a cada sessao
% Users = map Pid -> {Name,Thortled,{Num de pedidos no ultimo intervalo,lista de num de pedidos por intervalo, numero total de pedidos(sum da lista)}}
% Thortled = false or {true,time_remaning,is_logged} or true (caso estaja banido por outro sesrvidor de sessao)

% All_users = orSet de para saber o numero total de useres para efetuar o castigo do thortle
% Ban_user = orSet com todos os users que foram banidos no sistema para que ao trocar de sessao continuem banidos

% responsabilidade de retirar um cliente do ban e da sessao que o baniu 

session({Users,All_users,Ban_user,Sessions},Manager) ->
    receive
        {sessions,Sessions} ->
            io:format("Sessions: ~p~n",[lists:delete(self(),Sessions)]),
            Ret = {Users,All_users,Ban_user,Sessions};

        {request,Pid,T} ->
            {Nome,Trortled,{Num,L,Mean}} = maps:get(Pid,Users),
                case {Trortled,Num} of
                    {false,Num} ->  
                        request:request(T,Pid,Manager),
                        Map = maps:put(Pid,{Nome,Trortled,{Num + 1,L,Mean}},Users);
                    {{true,_,_},Num} when Num < ?MAX_MESSAGE ->
                        request:request(T,Pid,Manager),
                        Map = maps:put(Pid,{Nome,Trortled,{Num + 1,L,Mean}},Users);
                    {true,Num} when Num < ?MAX_MESSAGE ->
                        request:request(T,Pid,Manager),
                        Map = maps:put(Pid,{Nome,Trortled,{Num + 1,L,Mean}},Users);
                    {_,_} ->
                        Pid ! {resp,"false"},
                        Map = maps:put(Pid,{Nome,Trortled,{Num,L,Mean}},Users)
                end,
                Ret = {Map,All_users,Ban_user,Sessions};
            
        {timer} ->
            {{Ban_user1,Delta},Users1} = 
                maps:fold(fun(Key,Value,{{Acc_ban,Acc_ban_delta},Acc_user}) -> 
                    case Value of
                        {Nome,{_,_,Logged},_} -> Logged = Logged,Nome = Nome;
                        {Nome,Logged,_} -> Logged = Logged,Nome = Nome
                    end,
                    {B,S} = update_request(Value,Ban_user),
                    case B of
                        add -> 
                            {New_ban,New_delta} = orSet:add(Nome,self(),Acc_ban),
                            {{orSet:join(Acc_ban,New_ban),orSet:join(Acc_ban_delta,New_delta)},maps:put(Key,S,Acc_user)};
                        remove -> 
                            {New_ban,New_delta} = orSet:remove(Nome,Acc_ban),
                            Ret = {orSet:join(Acc_ban,New_ban),orSet:join(Acc_ban_delta,New_delta)},
                            case Logged of %remove utilizadores do mapa que tenham sido banidos mas não estejam no loged in
                                true -> {Ret,maps:put(Key,S,Acc_user)};
                                false -> {Ret,Acc_user}
                            end;
                        same -> {{Acc_ban,Acc_ban_delta},maps:put(Key,S,Acc_user)}
                    end 
                end,{{Ban_user,orSet:new()},#{}},Users),
            %io:format("Dic timer ~p ~nBan Users: ~p~nDelta: ~p~n",[Users1,orSet:elements(Ban_user1),Delta]),
            [Pid_s ! {ban_state,Delta} || Pid_s <- Sessions],
            Ret = {Users1,All_users,Ban_user1,Sessions};

        {login, Pid, Name} ->
            case orSet:is_element(Name,Ban_user) of
                true ->
                    {All_users1,Delta} = orSet:add(Name,self(),All_users),
                    Map = maps:put(Pid,{Name,true,{0,[],0}},Users); 
                false -> 
                    {All_users1,Delta} = orSet:add(Name,self(),All_users),
                    Map = maps:put(Pid,{Name,false,{0,[],0}},Users)
            end,
            [Pid_s ! {user_state,Delta} || Pid_s <- Sessions],
            Ret = {Map,All_users1,Ban_user,Sessions};
            
        {leave, Pid} ->
            io:format("Leave Dic: ~p~n",[Users]),
            {Name,T,S} = maps:get(Pid,Users),
            {All_users1,Delta} = orSet:remove(Name,All_users),
            case T of % caso o Pid esteja banido nao elemina a entrada ate o ban desaparecer
                {true,Time,_} -> New_users = maps:put(Pid,{Name,{true,Time,false},S},Users);
                _ -> New_users = maps:remove(Pid,Users) 
            end,
            [Pid_s ! {user_state,Delta} || Pid_s <- Sessions],
            Ret = {New_users,All_users1,Ban_user,Sessions};

        {user_state, Delta} ->
            All_users1 = orSet:join(Delta,All_users),
            Ret = {Users,All_users1,Ban_user,Sessions};

        {ban_state, Delta} -> 
            Ban_user1 = orSet:join(Delta,Ban_user),
            Ret = {Users,All_users,Ban_user1,Sessions}

    end,
    session(Ret,Manager).

update_request({Nome,T,{Num,List,_}},Ban_user) ->
    Intervale = round(?TIME / ?UPDATE),
    ListR = update_request_ban_state([Num|List],Intervale), 
    case {T,lists:sum(ListR)/?TIME} of
        {false,N} when N > ?LIMIT -> {add,{Nome,{true,60,true},{0,ListR,N}}};
        {false,N} -> {same,{Nome,T,{0,ListR,N}}};
        {{true,Time,Logged},N} when Time > ?UPDATE -> {same,{Nome,{true,Time-?UPDATE,Logged},{0,ListR,N}}};
        {{true,_,_},N} -> {remove,{Nome,false,{0,ListR,N}}};
        {true,N} ->
            case orSet:is_element(Nome,Ban_user) of
                true -> {same,{Nome,T,{0,ListR,N}}};
                false -> {same,{Nome,false,{0,ListR,N}}} 
            end  
    end.


update_request_ban_state([H|_],0) -> [H];
update_request_ban_state([H|T],Num) -> [H|update_request_ban_state(T,Num-1)];
update_request_ban_state([],_) -> [].

user(Sock,Session) -> 
    receive
        {resp,Data} -> 
            gen_tcp:send(Sock,Data),
            user(Sock,Session);
        {tcp,_,Data} ->
            inet:setopts(Sock, [{active, once}]),
            Parsed_data = parse_data(Data),
            %io:format("Parsed: ~p~n",[Parsed_data]),
            Send_data = proc_data(Parsed_data), 
            Session ! Send_data,
            case Send_data of %caso seja uma mensagem que de logout para não continuar o processo 
                {leave,_} -> ok;
                _ -> user(Sock,Session)
            end;
        {tcp_closed,_} ->
            io:format("Tcp closed~n",[]),
            Session ! {leave, self()};
        {tcp_error,_,_} -> 
            io:format("Tcp error~n",[]),
            Session ! {leave, self()};
        N -> io:format("Nao existe match -> ~p~n",[N])
    end.

proc_data(Data) ->
    case Data of
        ["li",Name|_] -> 
            io:format("recebeu login~n",[]),
            {login,self(),Name};
        ["lo"|_] -> 
            %io:format("recebeu logout~n",[]),
            {leave,self()};
        ["r"|T] -> 
            %io:format("recebeu read~n",[]),
            {request,self(),{read,T}};
        ["w"|T] -> 
            %io:format("recebeu write~n",[]),
            {request,self(),{write,T}};
        _ -> 
            io:format("recebeu mensagem n conhecida ~p~n",[Data]),
            {Data}
    end.

parse_data(Data) -> 
    lists:foldr(fun(Elem,Acc) -> split(Elem,Acc) end,[],Data).

split($\n,A) -> A;
split(0,A) -> A;    
split($|,[[]|T]) -> [[]|T];
split($|,A) -> [[]] ++ A;
split(Elem,[H|T]) -> [[Elem] ++ H|T];
split(Elem,[]) -> [[Elem]].


%% internal functions
