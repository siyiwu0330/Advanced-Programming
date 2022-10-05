-module(async).

-export([new/2, wait/1, poll/1, testFun/1]).

new(Fun, Arg) -> spawn(fun()->
    Me=self(),
    spawn(fun()->
        try 
            Res=Fun(Arg),
            Me!{ok,Res}
        catch
            _:Reason->Me!{error,Reason}
        end
   end),
loop({init})
end
).
wait(Aid) -> 
    case poll(Aid) of 
        nothing -> wait(Aid);
        {ok, Res}->Res;
        {exception,Reason}->throw(Reason)
    end.
        
poll(Aid) -> 
    Aid!{self(),getState},
    receive 
        {ok, Res} -> {ok, Res};
        {error,Reason} -> {exception,Reason };
        {nothing}->nothing
    % after 0 
    %     -> nothing 
    end.
loop(State)->
    receive 
        {ok,Res} -> loop({ok,Res});
        {error,Reason} -> loop({error,Reason});
        {From,getState}-> 
            case State of
                {init} -> From!{nothing},loop(State);
                _->From!State,loop(State)
            end
    end.

testFun(Name) ->
    io:format("Hello! ~p~n",[Name]).