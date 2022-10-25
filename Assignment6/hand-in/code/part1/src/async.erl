-module(async).

-export([new/2, wait/1, poll/1, wait_catch/1, wait_any/1, try_execute/3]).

-export([init/1, handle_call/3, handle_cast/2]).
-behaviour(gen_server).

new(Fun, Arg) -> 
    {ok,Aid}=gen_server:start(?MODULE,{Fun, Arg, nobody, false, none, none},[]),
    Aid.

wait(Aid) -> 
    case gen_server:call(Aid, wait_trans, infinity) of
        {ok, Val} -> Val;
        {exception, Execption} -> throw(Execption) %continue to throw if get exception
    end.

poll(Aid) -> 
    case gen_server:call(Aid, poll_trans) of
        {ok, Val} -> {ok, Val};
        {exception, Execption} -> {exception, Execption};
        nothing -> nothing
    end.
wait_catch(Aid) -> 
    case gen_server:call(Aid, wait_trans, infinity) of
        {ok, Val} -> {ok, Val};
        {exception, Execption} -> {exception, Execption} 
    end.

wait_any(Aids) -> %referring the Concurrent Fibonacci in "erlang-intro.pdf"
%using the Super here to make the message queue not that long
%because Super only get message once
    Super = self(), 
    spawn(fun() ->  
        Me = self(),
        % WaitOne = fun wait/1, %make an alias for function wait
        % fun() ->  in spawn can use the variable from outer serveral layers of environment
        GenOne = fun(Aid) -> spawn(fun() -> 
            Res = gen_server:call(Aid, wait_trans, infinity),
            Me ! {Aid ,Res}
            end) end,
        lists:foreach(GenOne, Aids), %similar but not equal to map in haskell
        receive
            {Aid,Res} -> Super ! {Aid, Res}
        end
    end),
    receive
        {Aid, {ok, Val}} -> {Aid, Val};
        {_, {exception, Execption}} -> throw(Execption) %continue to throw if get exception   
    end.

init({Fun, Arg, WaitingP, IsFinished, Result, _}) ->
    Me = self(), % Me is just the Pid of the gen_server
    Pid = spawn(async, try_execute, [Fun, Arg, Me]),
    {ok, {WaitingP, IsFinished, Result, Pid}}. % the big bracket after ok is the beginning state of server

handle_call(wait_trans, _From, {WaitingP, IsFinished, Result, Pid}) ->
    case IsFinished of
        true -> {reply, Result, {WaitingP, IsFinished, Result, Pid}};
        false ->{noreply,{_From, IsFinished, Result, Pid}}
    end;
handle_call(poll_trans, _From, {WaitingP, IsFinished, Result, Pid}) ->
    case IsFinished of
        true -> {reply, Result, {WaitingP, IsFinished, Result, Pid}};
        false ->{reply, nothing, {WaitingP, IsFinished, Result, Pid}}
    end.


handle_cast({try_execute_trans, Result},State) -> 
    case State of
        {WaitingP, _, _, Pid}->
            if 
                WaitingP =/= nobody ->
                    gen_server:reply(WaitingP, Result),
                    {noreply,{nobody, true, Result, Pid}};
                WaitingP =:= nobody ->
                    {noreply,{nobody, true, Result, Pid}}
            end
    end.


try_execute(Fun, Arg, ServerAid)->
    Result = 
        try Fun (Arg) of
            Val -> {ok, Val}
        catch
            throw : Exception -> {exception, Exception};
            exit : _TimeoutExit -> {exit, _TimeoutExit};
            error : _SomeErrors -> {error, _SomeErrors}
        end,
    gen_server:cast(ServerAid, {try_execute_trans, Result}).