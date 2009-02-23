-module(area_server_final).
-export([start/0,area/2]).


start()->
    spawn(fun loop/0).

area(Pid,What)->
    rpc(Pid, What).

loop()->
    receive
	{From,{rectangle,Height,Width}}->
	    From!{self(),Height*Width},
	    loop();
	{From,{circle,R}} ->
	    From ! {self(), 3.14*R*R},
	    loop();
	{From,Other} ->
	    From!{self(), {error,Other}},
	    loop()
    end.

rpc(Pid,Request)->
    Pid!{self(), Request},
    receive
	{Pid, Response}->
	    Response
    end.
