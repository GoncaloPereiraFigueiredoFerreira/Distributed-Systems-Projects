-module(orSet).

-export([new/0,elements/1,add/3,remove/3,join/1,join/2]).

% orSet = {Map(elem -> Set({id,c[id]})),CasualContext}

new() -> 
	{#{},#{}}.

elements({Map,_}) -> 
	maps:keys(Map).

add(Elem,Id,{Map,CC}) ->
	case maps:find(Id,CC) of
		error -> 
			CC1 = maps:put(Id,1,CC),
			Map1 = maps:put(Elem,sets:add_element({Id,1},sets:new()),Map),
			{{Map1,CC1},{maps:put(Elem,sets:add_element({Id,1},sets:new()),#{}),maps:put(Id,1,#{})}};
		{ok,Val} ->
			CC1 = maps:put(Id,Val + 1,CC),
			Map1 = maps:put(Elem,sets:add_element({Id,Val + 1},sets:new()),Map),
			{{Map1,CC1},{maps:put(Elem,sets:add_element({Id,Val + 1},sets:new()),#{}),maps:put(Id,Val + 1,#{})}}
	end.

remove(Elem,Id,{Map,CC}) -> 
	Set = maps:get(Elem,Map),
	case sets:to_list(sets:filter(fun({Set_id,_}) ->io:format("Id: ~p | Set_id: ~p | Result: ~p~n",[Id,Set_id,Id == Set_id]) ,Id == Set_id end,Set)) of
		[{Id,Clock}] -> 
			Delta = 
				maps:filtermap(fun(Key,Value) -> 
					not sets:is_empty(sets:filter(fun({Set_id,Val}) -> 
						P1 = Id == Set_id, P2 = Set_id /= Key,P3 = Val < Clock, P1 and P2 and P3 
					end,Value)) 
				end,Map),
			Del_CC = maps:put(Id,Clock,#{});
		_ ->
			Delta = #{},
			Del_CC = #{}
	end,
	{{maps:remove(Elem,Map),CC},{Delta,Del_CC}}.


join([]) -> empty;
join([Or_set|[]]) -> Or_set;
join([Or_set1,Or_set2|T]) -> R = join(Or_set1,Or_set2),join([R|T]).

join({Map1,CC1},{Map2,CC2}) -> 
	Map3 = merge_with(fun(_,Val1,Val2) -> {Set,_} = dotSet:join({Val1,CC1},{Val2,CC2}),Set end,Map1,Map2),
	U_cc = maps:merge_with( fun(_,Val1,Val2)->
								case Val1 > Val2 of
									true  -> Val1;
									false -> Val2
								end
							end,CC1,CC2),
	MapF = maps:filter(fun(_,Val) -> not sets:is_empty(Val) end,Map3),
	{MapF,U_cc}.


merge_with(Fun,Map1,Map2) -> 
	List = sets:to_list(sets:from_list(maps:keys(Map1) ++ maps:keys(Map2))),
	merge_with(Fun,Map1,Map2,#{},List).

merge_with(_,_,_,Map3,[]) -> Map3;

merge_with(Fun,Map1,Map2,Map3,[H|T]) ->
	case {maps:find(H,Map1),maps:find(H,Map2)} of
		{{ok,V1},{ok,V2}} -> merge_with(Fun,Map1,Map2,maps:put(H,Fun(H,V1,V2),Map3),T);
		{{ok,V1},error} -> merge_with(Fun,Map1,Map2,maps:put(H,Fun(H,V1,sets:new()),Map3),T);
		{error,{ok,V2}} -> merge_with(Fun,Map1,Map2,maps:put(H,Fun(H,sets:new(),V2),Map3),T);
		_ -> merge_with(Fun,Map1,Map2,Map3,T)
	end.
