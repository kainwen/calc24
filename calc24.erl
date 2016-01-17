-module(calc24).
-compile(export_all).
%-export([calc24/1]).

-define(OPS, [op_add, op_sub, op_mult, op_div]).


% export apis

calc24(L) when length(L) == 4 ->
    S1 = [L],
    S2 = transfor(S1), % length=3
    S3 = transfor(S2), % length=2
    S4 = transfor(S3),
    R24 = rational:make_rational(24),
    R = lists:filter(fun ([E]) -> rational:requal(eval_exp(E), R24) end, S4),
    SR = [exp_to_string(E) || [E] <- R],
    Result = sets:to_list(sets:from_list(SR)),
    lists:foreach(fun (S) -> io:format("~s~n", [S]) end, Result).

% internel help function

%% generate exps from two number(exps)

safe_exp(op_add, E1, E2) -> [{op_add, E1, E2}];
safe_exp(op_sub, E1, E2) -> [{op_sub, E1, E2}, {op_sub, E2, E1}];
safe_exp(op_mult, E1, E2) -> [{op_mult, E1, E2}];
safe_exp(op_div, E1, E2) -> [{op_div, E1, E2}, {op_div, E2, E1}].

generate_exps(A, B) ->
    lists:flatmap(fun (Op) -> safe_exp(Op, A, B) end, ?OPS).
		  
%% n choose 2(divide a list into 2 sublist)
nchoose2([A, B]) -> [{{A, B}, []}];
nchoose2([A | B]) when length(B) > 1 -> 
    WithoutA = [{S, [A|R]} || {S, R} <- nchoose2(B)],
    WithA = [{{A, S}, R} || {S, R} <- select_one(B)],
    WithA ++ WithoutA.    

select_one([A]) -> [{A, []}];
select_one([A|B]) ->
    WithA = {A, B},
    Rem = [{S, [A|R]} || {S, R} <- select_one(B)],
    [WithA | Rem].

%% transfor one
transfor_one(L) ->
    lists:flatmap(fun ({{A, B}, R}) ->
		    [[E|R] || E <- generate_exps(A, B)]
		  end,
		  nchoose2(L)).

transfor(L) ->
    lists:flatmap(fun transfor_one/1, L).

%% eval exp
eval_exp(N) when is_integer(N) -> rational:make_rational(N);
eval_exp({Op, E1, E2}) ->
    V1 = eval_exp(E1),
    V2 = eval_exp(E2),
    case {V1, V2} of
	{nil, _} -> nil;
	{_, nil} -> nil;
	_ ->
	    case Op of
		op_add -> rational:radd(V1, V2);
		op_sub -> rational:rsub(V1, V2);
		op_mult -> rational:rmult(V1, V2);
		op_div ->
		    R0 = rational:make_rational(0),
		    case rational:requal(R0, V2) of
			true -> nil;
			false -> rational:rdiv(V1, V2)
		    end
	    end
    end.
	    
exp_to_string(E) when is_integer(E) -> integer_to_list(E);
exp_to_string({Op, E1, E2}) ->
    S1 = exp_to_string(E1),
    S2 = exp_to_string(E2),
    case Op of
	op_add -> "( "  ++ S1 ++ " + " ++ S2 ++ " )";
	op_sub -> "( "  ++ S1 ++ " - " ++ S2 ++ " )";
	op_mult -> "( "  ++ S1 ++ " * " ++ S2 ++ " )";
	op_div -> "( "  ++ S1 ++ " / " ++ S2 ++ " )"
    end.
	    
