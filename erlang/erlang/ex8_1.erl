-module(ex8_1).
-export([start/2,start_process_server/0,testfun/0]).


start(AnAtom,Fun) ->
    ex8_1 ! {self(),AnAtom,Fun}.

start_process_server()->
    case whereis(ex8_1) of
        undefined -> register(ex8_1,spawn(fun()->process_server() end));
	_ -> whereis(ex8_1)
    
    end.


create(AnAtom,Fun) when is_pid(AnAtom)->
    {error,io:format("Process for atom ~p already registered.~n",[AnAtom])};
create(AnAtom,Fun)->
    register(AnAtom,spawn(fun()->Fun()end)).


process_server()->
    receive
	{From,AnAtom,Fun}->
	    From ! create(AnAtom,Fun),
	    process_server()
    end.


testfun()->
    testfun().
