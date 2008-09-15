-module(mylists).
-export([map/2, sum/1]).

map(_, [])->
    [];
map(F,[H|T]) ->[F(H)|map(F,T)].

sum([])->0;
sum([H|T])->
    H + sum(T).

