-module(rational).
-export([make_rational/1, make_rational/2, radd/2, rsub/2, rmult/2, rdiv/2, requal/2]).


% export apis

make_rational(N) when is_integer(N) -> {N, 1}.

make_rational(N, M) when is_integer(N) and is_integer(M) ->
   C = gcd(N, M),
   {N div C, M div C}.

radd({A, B}, {C, D}) -> make_rational(A*D+B*C, B*D).
rsub({A, B}, {C, D}) -> make_rational(A*D-B*C, B*D).
rmult({A, B}, {C, D}) -> make_rational(A*C, B*D).
rdiv({A, B}, {C, D}) -> make_rational(A*D, B*C).

requal({A, B}, {C, D}) -> (A == C) and (B == D);
requal(_, _) -> false.

% internel helper functions

gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).     
