%%%-------------------------------------------------------------------
%% @doc sessionNode public API
%% @end
%%%-------------------------------------------------------------------

-module(sessionNode_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    sessionNode_sup:start_link(),
    main(5555).

stop(_State) ->
    ok.

main(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [{active, 10}, list ,{packet,0},
                                        {reuseaddr, true}]),
    Client_manager = spawn(fun() -> client_manager(#{}) end),
    spawn(fun() -> acceptor(LSock,Client_manager) end),
    ok.

% Actor responsible for accepting new client connections
acceptor(LSock,Client_manager) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    ok = gen_tcp:controlling_process(Sock,Client_manager),
    Client_manager ! {new,Sock},
    acceptor(LSock,Client_manager).


storage_manager(Cache) ->
    storage_manager(Cache).


client_manager(Sessions) ->
    Self = self(),
    receive
        {new,Sock} ->
            User = spawn(fun() -> user(Sock,"",Self) end),
            ok = gen_tcp:controlling_process(Sock,User),
            client_manager(Sessions);

        _ ->
            io:format("fodasse ~n",[])
    end.




user(Sock,Name,Client_manager) ->
    Self = self(),
    receive
      {line, {Self, Data}} ->
        io:format("user received ~n",[]),
        inet:setopts(Sock, [{active, once}]),
        gen_tcp:send(Sock, Data),
        user(Sock, Name, Client_manager);

    
      {tcp, _, "login " ++ N} ->
        io:format("received ~p~n", [N]),
        Client_manager ! {"login",N},
        user(Sock,N,Client_manager);
      
      {tcp, _, "logout"} ->
        Client_manager ! {"logout",Name},
        user(Sock,Name,Client_manager);
      
      {tcp, _, "read"} ->
        Client_manager ! {"read",Name},
        user(Sock,Name,Client_manager);
      
      {tcp, _, "write"} ->
        Client_manager ! {"write",Name},
        user(Sock,Name,Client_manager);

      {tcp, _, Data} ->
        io:format("received ~p~n", [Data]),
        user(Sock,Name,Client_manager);

      _ ->
        io:format("fodasse ~n",[]),
        user(Sock,Name,Client_manager)
  
    end.


%% internal functions
