-module(magic).
-export([start/0,magic/1]).

start()->
    register(magicid,spawn(fun()->wait()end)).


magic(F)->
    magicid!{eval,self(),F},
    receive
	{magicid, Ret}->
	    Ret
    end.


wait()->
    receive
	{eval, Pid, F}->
	    Parent = self(),
	    spawn(fun()->
		      Pid!{magicid,F()},
		      Parent!free
		  end),
	    busy()
    end.

	
busy()->
    receive
	{eval, Pid, F}->
	    exit(Pid, {eNotSim}),
	    busy();
	free ->
	    wait()
    end.
