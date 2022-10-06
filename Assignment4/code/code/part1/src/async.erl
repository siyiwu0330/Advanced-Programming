-module(async).

-export([new/2, poll/1, wait/1, testFun/1, start/0]).

new(Fun, Arg) ->
    spawn(fun() ->
        Aid = self(),
        runFun(Fun, Arg, Aid),
        loop({runing})
    end).

runFun(Fun, Arg, Aid) ->
    spawn(fun() ->
        try Fun(Arg) of
            Res     -> Aid!{ok, Res}
        catch
            _:Error -> Aid!{error, Error}
        end
    end).

poll(Aid) ->
    Aid!{self(), poll},
    receive
        {ok, Res}      -> {ok, Res};
        {error, Error} -> {exception, Error};
        {nothing}      -> nothing
    end.

wait(Aid) ->
    Aid!{self(), wait},
    receive
        {ok, Res}       -> Res;
        {error, Error}  -> throw(Error);
        {nothing}       -> wait(Aid)
    end.

loop(State) ->
    receive
        {ok, Res}      -> loop({ok, Res});
        {error, Error} -> loop({error, Error});
        {From, poll} ->
            case State of
                {runing} -> From!{nothing}, loop(State);
                _        -> From!State, loop(State)
            end;
        {From, wait} ->
            case State of
                {runing} -> From!{nothing}, loop(State);
                _        -> From!State, loop(State)
            end
    end.

testFun(Timer) ->
    timer:sleep(Timer),
    io:format("*Function: sleep 1: ~w second ~n",[Timer/1000]),
    timer:sleep(Timer),
    io:format("*Function: sleep 2: ~w second ~n",[Timer/1000]),
    timer:sleep(Timer),
    io:format("*Function: sleep 3: ~w second ~n",[Timer/1000]),
    timer:sleep(Timer),
    io:format("*Function: sleep 4: ~w second ~n",[Timer/1000]).

start() -> 
    Aid = new(fun(MyTime) -> testFun(MyTime) end, 1000),
    case poll(Aid) of
        {ok, _} -> io:format("func finished~n", []);
        {error, Reason1} -> io:format("poll geterror~p~n",[Reason1]);
        nothing -> io:format("func is runing~n",[])
    end,
    timer:sleep(1000),
    case poll(Aid) of
        {ok, _} -> io:format("func finished~n", []);
        {error, Reason2} -> io:format("poll geterror~p~n",[Reason2]);
        nothing -> io:format("func is runing~n",[])
    end,
    timer:sleep(1000),
    case poll(Aid) of
        {ok, _} -> io:format("func finished~n", []);
        {error, Reason3} -> io:format("poll geterror~p~n",[Reason3]);
        nothing -> io:format("func is runing~n",[])
    end,
    io:format("wait~n",[]),
    wait(Aid),
    case poll(Aid) of
        {ok, _} -> io:format("func finished~n", []);
        {error, Reason4} -> io:format("poll geterror~p~n",[Reason4]);
        {nothing} -> io:format("fun is runing~n",[])
    end.
