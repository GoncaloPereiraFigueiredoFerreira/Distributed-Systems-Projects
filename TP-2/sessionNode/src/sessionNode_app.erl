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
    Session = spawn(fun() -> session(#{}) end),
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

% Id_user = {All_users,Ban_user,My_ban}
% All_users = orSet de para saber o numero total de useres para efetuar o castigo do thortle
% Ban_user = orSet com todos os users que foram banidos no sistema para que ao trocar de sessao continuem banidos
% My_ban = lsita especifica a cada sessao para saber quais os clientes que estao banidos por esta sessao

% responsabilidade de retirar um cliente do ban e da sessao que o baniu 

session(Users) ->
    receive
        {sessions,Sessions} ->
            io:format("Sessions: ~p~n",[Sessions]),
            session(Users);
        {request,Pid,T} ->
            {Nome,Trortled,{Num,L,Mean}} = maps:get(Pid,Users),
                case {Trortled,Num} of
                    {false,Num} ->  
                        Pid ! {resp,"ok"},
                        session(maps:put(Pid,{Nome,false,{Num + 1,L,Mean}},Users));
                    {{true,_},Num} when Num < ?MAX_MESSAGE ->
                        Pid ! {resp,"ok"},
                        session(maps:put(Pid,{Nome,Trortled,{Num + 1,L,Mean}},Users));
                    {_,_} ->
                        Pid ! {resp,"false"},
                        session(maps:put(Pid,{Nome,Trortled,{Num,L,Mean}},Users))
                end;
            
        {timer} ->
            Users1 = maps:map(fun(_,Value) -> update_request(Value) end,Users),
            io:format("Dic timer ~p~n",[Users1]),
            session(Users1);
        {login, Pid, Name} ->
            session(maps:put(Pid,{Name,false,{0,[],0}},Users));
        {leave, Pid} ->
            session(maps:remove(Pid,Users))
    end.

update_request({Nome,T,{Num,List,_}}) ->
    Intervale = ?TIME div ?UPDATE,
    ListR = update_request([Num|List],Intervale), 
    case {T,lists:sum(ListR)/60} of
        {false,N} when N > ?LIMIT -> {Nome,{true,60},{0,ListR,N}};
        {false,N} -> {Nome,T,{0,ListR,N}};
        {{true,Time},N} when Time > ?UPDATE -> {Nome,{true,Time-?UPDATE},{0,ListR,N}};
        {{true,_},N} -> {Nome,false,{0,ListR,N}}
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
        ["login",Name|_] -> 
            io:format("recebeu login~n",[]),
            {login,self(),Name};
        ["logout"|_] -> 
            io:format("recebeu logout~n",[]),
            {leave,self()};
        ["read"|T] -> 
            io:format("recebeu read~n",[]),
            {request,self(),{read,T}};
        ["write"|T] -> 
            io:format("recebeu write~n",[]),
            {request,self(),{write,T}};
        _ -> 
            io:format("recebeu mensagem n conhecida ~p~n",[Data]),
            {Data}
    end.

parse_data(Data) -> 
    lists:foldr(fun(Elem,Acc) -> split(Elem,Acc) end,[],Data).

split($\n,A) -> A;    
split($ ,[[]|T]) -> [[]|T];
split($ ,A) -> [[]] ++ A;
split(Elem,[H|T]) -> [[Elem] ++ H|T];
split(Elem,[]) -> [[Elem]].


%% internal functions
