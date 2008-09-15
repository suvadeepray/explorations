-module(shop2).
-import(lists,[map/2,sum/1]).
-export([total/1]).

total(L)->
    sum(map(fun({What,N})->
		    shop:cost(What)*N end, L)).
