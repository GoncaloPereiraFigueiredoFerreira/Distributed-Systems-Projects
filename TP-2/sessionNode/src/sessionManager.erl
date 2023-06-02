-module(sessionManager).

-export([start/0,add_session/1,add_session/1,create_session/1]).

start() -> 
	register(?MODULE,spawn(fun() -> sessionManager([]) end)),
	ok.

sessionManager(Sessions) -> 
	[Pid ! {sessions,Sessions} || Pid <- Sessions],
	receive
		{enter,Pid} -> Sessions1 = [Pid|Sessions];
		{leave,Pid} -> Sessions1 = lists:delete(Pid)
	end,
	io:format("Manager: Sessions ~p~n",[Sessions1]),
	sessionManager(Sessions1).

add_session(Pid) ->
	?MODULE ! {enter,Pid}.

create_session(Port) -> 
	sessionNode_app:main(Port).