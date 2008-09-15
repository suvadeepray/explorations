-module(geometry).
-export([area/1]).

area({rectangle,Width,Ht})->
    Width*Ht;
area({circle, R}) -> R*R*3.14159;
area({square,X}) ->X*X.
