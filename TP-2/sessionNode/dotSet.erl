-module(dotSet).
-export([join/1,join/2]).

% dot set = {Set,CasualContext}

% join exposto para fora do modulo recebe uma lista de dot set que da join
join([]) -> empty;
join([Dot_set|[]]) -> Dot_set;
join([Dot_set1,Dot_set2|T]) -> R = join(Dot_set1,Dot_set2),join([R|T]).

%join que junta dois Dot Sets

join({Set1,CC1},{Set2,CC2}) -> 
	I_set = sets:intersection(Set1,Set2),
	S1 = sets:union(sets:filter(fun(X)->filter_fun(X,CC2) end,Set1),I_set),%remover ao set 1 o CC2
	S2 = sets:union(sets:filter(fun(X)->filter_fun(X,CC1) end,Set2),S1),%remover ao set 2 o CC1
	U_cc = maps:merge_with( fun(_,Val1,Val2)->
								case Val1 > Val2 of
									true  -> Val1;
									false -> Val2
								end
							end,CC1,CC2),
	{S2,U_cc}.

filter_fun({Id,V1},Map) ->
	case maps:find(Id,Map) of
		{ok,V2} -> V1 >= V2;
		_       -> true
	end.

