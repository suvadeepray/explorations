
%%%-------------------------------------------------------------------
%%% File    : tokenring.erl
%%% Author  : vikrant <vikrant@nile>
%%% Description : this module implements token ring protocol for benchmarking
%%% 
%%% Created : 28 Mar 2009 by vikrant <vikrant@nile>
%%%-------------------------------------------------------------------
-module(tokenring).
-export([tokenring/2]).


tokenring(N,M)->
    [Head | Tail] = for(1,N, fun()-> spawn(fun()-> wait() end) end),
    TransposeByOne = lists:append(Tail,[Head]),
    Pairs = lists:zip([Head | Tail], TransposeByOne),
    lists:map(fun({To, Arg})-> To! {token, Arg, M*N} end, Pairs),
    statistics(runtime),
    statistics(wall_clock),
    Head ! {token, self(), 0},
    receive
	done ->
	    void
    end,
    {_,Time1} = statistics(runtime),
    {_,Time2} = statistics(wall_clock),
    U1 = Time1*1000,
    U2 = Time2*1000,
    lists:foreach(fun(Pid)-> Pid!die end, [Head | Tail]),
    io:format("Time required to pass message ~p times=~p (~p) microsecond~n.",
	      [M*N,U1,U2]).

    


loop(To, MaxCount) ->
    receive
	{token,Parent,Count} when Count < MaxCount ->
	    %io:format("Token passing from ~p to ~p~n",[self(), To]),
	    To! {token, Parent, Count+1},
	    loop(To, MaxCount);
	{token,Parent, MaxCount} ->
	    Parent ! done,
	    loop(To, MaxCount);
	die ->
	    void
    end.


wait() ->
    receive
	{token, To, MaxCount} ->
	    loop(To, MaxCount)
    end.


for(N,N,F) -> [F()];
for(I,N,F) -> [F() | for(I+1,N,F)].
