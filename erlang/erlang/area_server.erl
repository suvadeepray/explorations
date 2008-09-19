-module(area_server).
-export([loop/0,loop2/0,rpc/2]).

loop() ->
    receive
	{rectangle, Width, Ht} ->
	    io:format("Area of rectangle is ~p~n",[Width*Ht]),
	    loop();
	{circle, R} ->
	    io:format("Area of circle is ~p~n",[3.14159*R*R]),
	    loop();
	Other ->
	    io:format("I don't know what the area of a ~p is ~n",[Other]),
	    loop()
    end.


loop2() ->
    receive
	{From,{rectangle, Width, Ht}} ->
	    From ! {self(), Width*Ht},
	    loop2();
	{From, {circle, R}} ->
	    From ! {self(), 3.14159*R*R},
	    loop2();
	{From, Other} ->
	    From ! {self(), "I don't know what the area of the given shape! Exiting.."}
    end.

rpc(Pid, Request)->
    Pid ! {self(), Request},
    receive
	{Pid, Response}->
	    Response
    end.

