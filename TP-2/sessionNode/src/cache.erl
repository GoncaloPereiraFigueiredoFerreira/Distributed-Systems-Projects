-module(cache).

-export([start/0,request/4,request/5]).

-define(LIMIT_CACHE, 1000).

start() -> 
	spawn(fun() -> cache_manager(#{},0) end).

request(read,Key,Version,S) ->
	S ! {read,self(),{Key,Version}},
	receive
		{ok,Val} -> Val;
		error -> error
	end.

request(write,Key,Version,Valor,S) ->
	S ! {write,self(),{Key,Version},Valor}.

cache_manager(Cache,Num) -> 
	receive
		{read,Pid,Key} -> 
			Pid ! maps:find(Key,Cache),
			Cache1 = Cache, Num1 = Num;

		{write,Pid,Key,Valor} when Num >= ?LIMIT_CACHE -> 
			case maps:find(Key,Cache) of
				{ok,_} -> Cache1 = Cache,Num1 = Num;
				error -> Cache1 = maps:put(Key,Valor,delete_random(Cache)),Num1 = Num
			end;

		{write,Pid,Key,Valor} -> 
			case maps:find(Key,Cache) of
				{ok,_} -> Cache1 = Cache,Num1 = Num;
				error -> Cache1 = maps:put(Key,Valor,Cache),Num1 = Num + 1
			end
	end,
	cache_manager(Cache1,Num1).

delete_random(Cache) ->
	Key = lists:nth(random:uniform(?LIMIT_CACHE),maps:keys(Cache)),
	maps:remove(Key,Cache).