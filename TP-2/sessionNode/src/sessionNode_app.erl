%%%-------------------------------------------------------------------
%% @doc sessionNode public API
%% @end
%%%-------------------------------------------------------------------

-module(sessionNode_app).

-behaviour(application).

-export([start/2, stop/1,split/2]).

-define(TIME, 60).
-define(UPDATE, 1).
-define(LIMIT, 1000).
-define(MAX_MESSAGE, 100).


start(_StartType, _StartArgs) ->
    sessionNode_sup:start_link(),
    main(5555).

stop(_State) ->
    ok.

main(Port) ->
    io:format("Sessao na porta ~p~n",[Port]),
    {ok, LSock} = gen_tcp:listen(Port, [{active, once}, {packet, line},{reuseaddr, true}]),
    Session = spawn(fun() -> session(#{},orSet:new(),orSet:new()) end),
    spawn(fun() -> timer(?UPDATE,Session) end),
    %Manager ! {enter,Session},
    acceptor(LSock, Session),
    ok.

timer(Time,Pid) ->
    receive
        after Time * 1000 ->
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
% Thortled = false or {true,time_remaning}

% All_users = orSet de para saber o numero total de useres para efetuar o castigo do thortle
% Ban_user = orSet com todos os users que foram banidos no sistema para que ao trocar de sessao continuem banidos

% responsabilidade de retirar um cliente do ban e da sessao que o baniu 

session(Users,All_users,Ban_user) ->
    receive
        {sessions,Sessions} ->
            io:format("Sessions: ~p~n",[Sessions]),
            session(Users,All_users,Ban_user);
        {request,Pid,T} ->
            {Nome,Trortled,{Num,L,Mean}} = maps:get(Pid,Users),
                case {Trortled,Num} of
                    {false,Num} ->  
                        Pid ! {resp,"ok"},
                        session(maps:put(Pid,{Nome,false,{Num + 1,L,Mean}},Users),All_users,Ban_user);
                    {{true,_},Num} when Num < ?MAX_MESSAGE ->
                        Pid ! {resp,"ok"},
                        session(maps:put(Pid,{Nome,Trortled,{Num + 1,L,Mean}},Users),All_users,Ban_user);
                    {_,_} ->
                        Pid ! {resp,"false"},
                        session(maps:put(Pid,{Nome,Trortled,{Num,L,Mean}},Users),All_users,Ban_user)
                end;
            
        {timer} ->
            {{Ban_user1,Delta},Users1} = 
                maps:fold(fun(Key,Value,{{Acc_ban,Acc_ban_delta},Acc_user}) -> 
                    {B,S} = update_request(Value),
                    case B of
                        add -> 
                            {New_ban,New_delta} = orSet:add(Key,self(),Acc_ban),
                            {{orSet:join(Acc_ban,New_ban),orSet:join(Acc_ban_delta,New_delta)},maps:put(Key,S,Acc_user)};
                        remove -> 
                            {New_ban,New_delta} = orSet:remove(Key,Acc_ban),
                            {{orSet:join(Acc_ban,New_ban),orSet:join(Acc_ban_delta,New_delta)},maps:put(Key,S,Acc_user)};
                        same -> {{Acc_ban,Acc_ban_delta},maps:put(Key,S,Acc_user)}
                    end 
                end,{{Ban_user,orSet:new()},#{}},Users),
            io:format("Dic timer ~p ~nBan Users: ~p~nDelta: ~p~n",[Users1,orSet:elements(Ban_user1),Delta]),
            session(Users1,All_users,Ban_user1);
        {login, Pid, Name} ->
            {All_users1,Delta} = orSet:add(Name,self(),All_users), 
            session(maps:put(Pid,{Name,false,{0,[],0}},Users),All_users1,Ban_user);
        {leave, Pid} ->
            {Name,_,_} = maps:get(Pid,Users),
            {All_users1,Delta} = orSet:remove(Name,All_users),
            session(maps:remove(Pid,Users),All_users1,Ban_user)
    end.

update_request({Nome,T,{Num,List,_}}) ->
    Intervale = ?TIME div ?UPDATE,
    ListR = update_request([Num|List],Intervale), 
    case {T,lists:sum(ListR)/60} of
        {false,N} when N > ?LIMIT -> {add,{Nome,{true,60},{0,ListR,N}}};
        {false,N} -> {same,{Nome,T,{0,ListR,N}}};
        {{true,Time},N} when Time > ?UPDATE -> {same,{Nome,{true,Time-?UPDATE},{0,ListR,N}}};
        {{true,_},N} -> {remove,{Nome,false,{0,ListR,N}}}
    end.


update_request([H|_],0) -> [H];
update_request([H|T],Num) -> [H|update_request(T,Num-1)];
update_request([],_) -> [].

user(Sock,Session) -> 
    receive
        {resp,Data} -> 
            gen_tcp:send(Sock,Data),
            user(Sock,Session);
        {tcp,_,Data} ->
            inet:setopts(Sock, [{active, once}]),
            Parsed_data = parse_data(Data),
            io:format("Parsed: ~p~n",[Parsed_data]),
            Session ! proc_data(Parsed_data),
            user(Sock,Session);
        {tcp_close,_} ->
            Session ! {leave, self()};
        {rcp_error,_,_} -> 
            Session ! {leave, self()}
    end.

proc_data(Data) ->
    case Data of
        ["li",Name|_] -> 
            io:format("recebeu login~n",[]),
            {login,self(),Name};
        ["lo"|_] -> 
            io:format("recebeu logout~n",[]),
            {leave,self()};
        ["r"|T] -> 
            io:format("recebeu read~n",[]),
            {request,self(),{read,T}};
        ["w"|T] -> 
            io:format("recebeu write~n",[]),
            {request,self(),{write,T}};
        _ -> 
            io:format("recebeu mensagem n conhecida ~p~n",[Data]),
            {Data}
    end.

parse_data(Data) -> 
    lists:foldr(fun(Elem,Acc) -> split(Elem,Acc) end,[],Data).

split($\n,A) -> A;    
split($|,[[]|T]) -> [[]|T];
split($|,A) -> [[]] ++ A;
split(Elem,[H|T]) -> [[Elem] ++ H|T];
split(Elem,[]) -> [[Elem]].


%% internal functions
