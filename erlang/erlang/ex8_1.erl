
-module(ex8_1).
-export([start/2,start_process_server/0,testfun/0,stop_process_server/0]).


start(AnAtom,Fun) ->
    ex8_1 ! {eval, self(),AnAtom,Fun},
    receive
	{result,Result}->
	    Result
    end.

stop_process_server()->
    ex8_1 ! stop.

start_process_server()->
    register(ex8_1,spawn(fun()->process_server() end)).


create(AnAtom,Fun) ->
    register(AnAtom,spawn(fun()->Fun()end)).

process_server()->
    receive
	{eval, From,AnAtom,Fun}->
	    Parent = self(),
	    spawn(fun()->
			  From ! {result, create(AnAtom,Fun)},
			  Parent ! free
		  end),
		  busy();
	stop ->
	    stop
    end.


busy()->
    receive
	{eval,From,AnAtom,Fun}->
	    exit(From, eNotSim);
	free ->
	    process_server()
    end.


testfun()->
    receive
	{From,X}->
	    From ! {self(), X},
	    testfun();
	stop ->
	    stop
    end.


