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
    Self = self(),
    {ok, LSock} = gen_tcp:listen(Port, [{active, once}, {packet, line},{reuseaddr, true}]),
    sessionManager:add_session(Self),
    spawn(fun() -> acceptor(LSock, Self) end),
    session({#{},orSet:new(),orSet:new(),[]},Request),
    ok.

acceptor(LSock, Session) ->
      {ok, Sock} = gen_tcp:accept(LSock),
      spawn(fun() -> acceptor(LSock, Session) end),
      Login = {_,_,Name} = login(Sock),
      Session ! Login,
      timer:send_interval(?UPDATE * 1000,{timer}),
      user(Sock,{Name,false,{0,[],0}},Session).

login(Sock) ->
    receive
        {tcp,_,Data} ->
            inet:setopts(Sock, [{active, once}]),
            case proc_data(parse_data(Data)) of
                {login,Pid,Name} -> gen_tcp:send(Sock, "ok"),{login,Pid,Name};
                _ -> gen_tcp:send(Sock, "err"),login(Sock)
            end
    end.

% Estado interno a cada sessao

% Thortled = false or {true,time_remaning,is_logged} or true (caso estaja banido por outro sesrvidor de sessao)
% Users = #{Name -> Pid}
% All_users = orSet de para saber o numero total de useres para efetuar o castigo do thortle
% Ban_user = orSet com todos os users que foram banidos no sistema para que ao trocar de sessao continuem banidos

% responsabilidade de retirar um cliente do ban e da sessao que o baniu 

session({Users,All_users,Ban_user,Sessions},Manager) ->
    receive
        {sessions,Sessions1} ->
            io:format("Sessions: ~p~n",[lists:delete(self(),Sessions1)]),
            Extra = Sessions1 -- Sessions,
            [Pid_s ! {user_state,All_users} || Pid_s <- Extra],
            [Pid_s ! {ban_state,Ban_user} || Pid_s <- Extra],
            Ret = {Users,All_users,Ban_user,lists:delete(self(),Sessions1)};

        {request,Pid,T} ->
            request:request(T,Pid,Manager),
            Ret = {Users,All_users,Ban_user,Sessions};

        {login, Pid, Name} ->
            case {orSet:is_element(Name,All_users),orSet:is_element(Name,Ban_user)} of
                {true,_} -> 
                    Pid ! {leave,"err"},
                    All_users1 = All_users,
                    Map = Users;
                {false,true} ->
                    {All_users1,Delta} = orSet:add(Name,self(),All_users),
                    [Pid_s ! {user_state,Delta} || Pid_s <- Sessions],
                    Pid ! {ban,true},
                    Map = maps:put(Name,Pid,Users);
                {false,false} -> 
                    {All_users1,Delta} = orSet:add(Name,self(),All_users),
                    [Pid_s ! {user_state,Delta} || Pid_s <- Sessions],
                    Map = maps:put(Name,Pid,Users)
            end,
            Ret = {Map,All_users1,Ban_user,Sessions};
            
        {leave, Name} ->
            {All_users1,Delta} = orSet:remove(Name,All_users),
            [Pid_s ! {user_state,Delta} || Pid_s <- Sessions],
            New_users = maps:remove(Name,Users),
            Ret = {New_users,All_users1,Ban_user,Sessions};

        {ban,Name,Bool} -> 
            case Bool of
                false -> {Ban_user1,Delta} = orSet:remove(Name,Ban_user);
                true -> {Ban_user1,Delta} = orSet:add(Name,self(),Ban_user)
            end,
            [Pid_s ! {ban_state,Delta} || Pid_s <- Sessions],
            Ret = {Users,All_users,Ban_user1,Sessions};

        {user_state, Delta} ->
            All_users1 = orSet:join(Delta,All_users),
            Ret = {Users,All_users1,Ban_user,Sessions};

        {ban_state, Delta} -> 
            Ban_user1 = orSet:join(Delta,Ban_user),
            {Add,Rem} = orSet:difference(Ban_user,Ban_user1),
            lists:foreach(fun(Elem) -> send_msg(Elem,Users,{ban,true}) end,Add),
            lists:foreach(fun(Elem) -> send_msg(Elem,Users,{ban,false}) end,Rem),
            Ret = {Users,All_users,Ban_user1,Sessions};

        {add_node,Pid} -> 
            request:request(add_node,Pid,Manager),
            Ret = {Users,All_users,Ban_user,Sessions}
    end,
    session(Ret,Manager).

send_msg(Elem,Map,Msg) -> 
    case maps:find(Elem,Map) of
         {ok,Val} -> Val ! Msg;
         error -> ok
     end. 

update_request({Nome,T,{Num,List,_}},N_user) ->
    Intervale = round(?TIME / ?UPDATE),
    ListR = update_request_ban_state([Num|List],Intervale), 
    case {T,lists:sum(ListR)/60} of
        {false,N} when N >= ?LIMIT -> {add,{Nome,{true,60 + N_user},{0,ListR,N}}};
        {false,N} -> {same,{Nome,T,{0,ListR,N}}};
        {{true,Time},N} when Time > ?UPDATE -> {same,{Nome,{true,Time-?UPDATE},{0,ListR,N}}};
        {{true,_},N} -> {remove,{Nome,false,{0,ListR,N}}};
        {true,N} -> {same,{Num,ListR,N}}
    end.


update_request_ban_state([H|_],0) -> [H];
update_request_ban_state([H|T],Num) -> [H|update_request_ban_state(T,Num-1)];
update_request_ban_state([],_) -> [].

leave({Nome,{true,N},Val},Session) -> 
    case N > 0 of
        true -> receive
                    {timer} -> leave({Nome,{true,N-1},Val},Session)
                end;
        false -> Session ! {ban,Nome,false}
    end;

leave(_,Session) -> ok.

user(Sock,User_data,Session) -> 
    receive
        {resp,Data} -> 
            io:format("~p~n",[Data]),
            gen_tcp:send(Sock,Data),
            user(Sock,User_data,Session);
        {leave,Data} -> 
            gen_tcp:send(Sock,Data),
            gen_tcp:shutdown(Sock,read_write),
            leave(User_data,Session);
        {timer} -> 
            {Name,_,_} = User_data,
            V = update_request(User_data,1),
            case V of
                {same,Val} -> user(Sock,Val,Session);
                {add,Val} -> 
                    Session ! {ban,Name,true},
                    user(Sock,Val,Session);
                {remove,Val} ->
                    Session ! {ban,Name,false}, 
                    user(Sock,Val,Session)
            end;
        {ban,Bool} ->  
            {Name,_,Val} = User_data,
            user(Sock,{Name,Bool,Val},Session);
        {tcp,_,Data} ->
            inet:setopts(Sock, [{active, once}]),
            Parsed_data = parse_data(Data),
            %io:format("Parsed: ~p~n",[Parsed_data]),
            Send_data = proc_data(Parsed_data),
            {Name,_,_} = User_data, 
            case Send_data of %caso seja uma mensagem que de logout para não continuar o processo 
                {login,_,_} -> user(Sock,User_data,Session);
                {leave,_} -> Session ! {leave,Name},self() ! {leave,"ok"};
                _ -> {Nome,Trortled,{Num,L,Mean}} = User_data,
                     case {Trortled,Num} of 
                        {false,Num} -> 
                            Session ! Send_data,
                            User_data1 = {Nome,Trortled,{Num + 1,L,Mean}};
                        {{true,_,_},Num} when Num < ?MAX_MESSAGE ->
                            Session ! Send_data,
                            User_data1 = {Nome,Trortled,{Num + 1,L,Mean}};
                        {true,Num} when Num < ?MAX_MESSAGE -> 
                            Session ! Send_data,
                            User_data1 = {Nome,Trortled,{Num + 1,L,Mean}};
                        {_,_} -> 
                            self() ! {resp,"false"},
                            User_data1 = User_data
                     end,
                     user(Sock,User_data1,Session)
            end;
        {tcp_closed,_} ->
            io:format("Tcp closed~n",[]),
            {Name,_,_} = User_data,
            Session ! {leave, Name},
            leave(User_data,Session);
        {tcp_error,_,_} -> 
            io:format("Tcp error~n",[]),
            {Name,_,_} = User_data,
            Session ! {leave, Name},
            leave(User_data,Session);
        N -> io:format("Nao existe match -> ~p~n",[N])
    end.

proc_data(Data) ->
    case Data of
        ["li",Name|_] -> 
            %io:format("recebeu login ~p~n",[Name]),
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
        ["a","d"|_] -> 
            {add_node,self()};
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
