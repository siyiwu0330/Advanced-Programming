-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1]).

% -type shortcode() :: string().
% -type emoji() :: binary().
% -type analytic_fun(State) :: fun((shortcode(), State) -> State).


start(Initial) ->
    ShortList =
        lists:map(
            fun(Emoji) ->
                case Emoji of
                    {Short, _} -> Short
                end
            end, Initial),
    case duplicate(ShortList) of
        true  -> {error, "duplicate shortcodes"};
        false ->
            try E = spawn(fun() -> loop(withBind(Initial)) end) of
                _        -> {ok, E}     
            catch
                _:Reason -> {error, Reason}
                    
            end
    end.


new_shortcode(E, Short, Emo) ->
    request_reply(E, {new_shortcode, Short, Emo}).

alias(E, Short1, Short2) ->
    request_reply(E, {alias, Short1, Short2}).

delete(E, Short) ->
    request_reply(E, {delete, Short}).

lookup(E, Short) ->
    request_reply(E, {lookup, Short}).

analytics(E, Short, Fun, Label, Init) ->
    request_reply(E, {analytics, Short, Fun, Label, Init}).

get_analytics(E, Short) ->
    request_reply(E, {get_analytics, Short}).

remove_analytics(E, Short, Label) ->
    request_reply(E, {remove_analytics, Short, Label}).

stop(E) ->
    request_reply(E, stop).


request_reply(Pid, Request) ->
	Pid ! {self(), Request},
	receive
		{Pid, Response} -> Response
	end.


loop(EMap) ->
    ShortList = 
        lists:map(
            fun(Emoji) ->
                case Emoji of
                    {Short, _, _, _} -> Short
                end
            end, EMap),
    receive
        {From, {new_shortcode, Short, Emo}} ->
            case lists:member(Short, ShortList) of
                true  ->
                    From!{self(), {error, "new_shortcode error: |"++ Short ++"| already exists"}},
                    loop(EMap);
                false ->
                    From!{self(), ok},
                    loop([{Short, Emo, Short, []}|EMap])
            end;
        {From, {alias, Short1, Short2}} ->
            case lists:member(Short1, ShortList) of
                false ->
                    From!{self(), {error, "alias error: |"++ Short1 ++"| does not exist"}},
                    loop(EMap);
                true  ->
                    case lists:member(Short2, ShortList) of
                        true  ->
                            From!{self(), {error, "alias error: |"++ Short2 ++"| already exists"}},
                            loop(EMap);
                        false ->
                            From!{self(), ok},
                            loop([{Short2, getEmo(Short1, EMap), realShort(Short1, EMap), []}|EMap])
                    end
            end;
        {From, {delete, Short}} ->
            case lists:member(Short, ShortList) of
                false ->
                    loop(EMap);
                true  ->
                    From!{self(), ok},
                    loop(deleteAll(realShort(Short, EMap), EMap))
            end;
        {From, {lookup, Short}} ->
            case lists:member(Short, ShortList) of
                false ->
                    From!{self(), no_emoji},
                    loop(EMap);
                true  ->
                    From!{self(), {ok, getEmo(Short, EMap)}},
                    loop(renewState(Short, realShort(Short, EMap), EMap))
            end;
        {From, {analytics, Short, Fun, Label, Init}} ->
            case lists:member(Short, ShortList) of
                false ->
                    From!{self(), {error, "analytics error: |"++ Short ++"| does not exist"}},
                    loop(EMap);
                true  ->
                    case labelExist(realShort(Short, EMap), Label, EMap) of
                        true  ->
                            From!{self(), {error, "analytics error: |"++ Label ++"| already exists"}},
                            loop(EMap);
                        false ->
                            From!{self(), ok},
                            loop(bindFun(realShort(Short, EMap), Fun, Label, Init, EMap))
                    end
            end;
        {From, {get_analytics, Short}} ->
            case lists:member(Short, ShortList) of
                false ->
                    From!{self(), {error, "get_analytics error: |"++ Short ++"| does not exist"}},
                    loop(EMap);
                true  ->
                    From!{self(), {ok, getStat(realShort(Short, EMap), EMap)}},
                    loop(EMap)
            end;
        {From, {remove_analytics, Short, Label}} ->
            case lists:member(Short, ShortList) of
                false ->
                    From!{self(), {error, "remove_analytics error: |"++ Short ++"| does not exist"}},
                    loop(EMap);
                true  ->
                    From!{self(), ok},
                    loop(removeFun(realShort(Short, EMap), Label, EMap))
            end;
        {From, stop} -> From!{self(), ok};
        {From, _} ->
            From!{self(), "syntax error"},
            loop(EMap)
    end.




duplicate(ShortList)->
    case ShortList of
        [] -> false;
        [Head|Tail] ->
            case lists:member(Head, Tail) of
                true  -> true;
                false -> duplicate(Tail)
            end
    end.

withBind(List) ->
    case List of
        [] -> [];
        [{Short, Emo}|Rest] -> [{Short, Emo, Short, []}|withBind(Rest)]
    end.

realShort(Short, EMap) ->
    case EMap of
        [] -> noting;
        [{Short_, _, RealShort, _}|Rest] ->
            case Short_ =:= Short of
                true  -> RealShort;
                false -> realShort(Short, Rest)
            end
    end.

getEmo(Short, EMap) ->
    case EMap of
        [] -> noting;
        [{Short_, Emo, _, _}|Rest] ->
            case Short_ =:= Short of
                true  -> Emo;
                false -> getEmo(Short, Rest)
            end
    end.

deleteAll(Short, EMap) ->
    case EMap of
        [] -> [];
        [{Short_, Emo, RealShort, FunList}|Rest] ->
            case RealShort =:= Short of
                true  -> deleteAll(Short, Rest);
                false -> [{Short_, Emo, RealShort, FunList}|deleteAll(Short, Rest)]
            end
    end.

renewState(Short, RealShort, EMap) ->
    case EMap of
        [] -> [];
        [{Short_, Emo, RealShort_, FunList}|Rest] ->
            case (RealShort =:= RealShort_) and (RealShort =:= Short_) of
                true  -> [{Short_, Emo, RealShort_, runFun(Short, FunList)}|Rest];
                false -> [{Short_, Emo, RealShort_, FunList}|renewState(Short, RealShort, Rest)]
            end
    end.

runFun(Short, FunList) ->
    case FunList of
        [] -> [];
        [{Fun, Label, State}|Rest] ->
            try Fun(Short, State) of
                NewState -> [{Fun,Label,NewState}|runFun(Short,Rest)]
            catch
                _:_      -> [{Fun,Label,State}|runFun(Short,Rest)]
            end
    end.

labelExist(Short, Label, EMap) ->
    case EMap of
        [] -> false;
        [{Short_, _, _, FunList}|Rest] ->
            case Short_ =:= Short of
                false -> labelExist(Short, Label, Rest);
                true  ->
                    case findLable(Label, FunList) of
                        true  -> true;
                        false -> labelExist(Short, Label, Rest)
                    end
            end
    end.

findLable(Label, FunList) ->
    case FunList of
        [] -> false;
        [{_, Label_, _}|Rest] ->
            case Label_ =:= Label of
                true  -> true;
                false -> findLable(Label, Rest)
            end
    end.

bindFun(Short, Fun, Label, Init, EMap) ->
    case EMap of
        [] -> [];
        [{Short_, Emo, RealShort, FunList}|Rest] ->
            case Short_ =:= Short of
                false -> [{Short_, Emo, RealShort, FunList}|bindFun(Short, Fun, Label, Init, Rest)];
                true  -> [{Short_, Emo, RealShort, [{Fun, Label, Init}|FunList]}|Rest]
            end
    end.

getStat(Short, EMap) ->
    case EMap of
        [] -> nothing;
        [{Short_, _, _, FunList}|Rest] ->
            case Short_ =:= Short of
                true  -> showStat(FunList);
                false -> getStat(Short, Rest)
            end
    end.

showStat(FunList) ->
    case FunList of
        [] -> [];
        [{_, Label, State}|Rest] -> [{Label, State}|showStat(Rest)]
    end.

removeFun(Short, Label, EMap) ->
    case EMap of
        [] -> [];
        [{Short_, Emo, RealShort, FunList}|Rest] ->
            case Short_ =:= Short of
                true  -> [{Short_, Emo, RealShort, remove(Label, FunList)}|Rest];
                false -> [{Short_, Emo, RealShort, FunList}|removeFun(Short, Label, Rest)]
            end
    end.

remove(Label, FunList) ->
    case FunList of
        [] -> [];
        [{Fun, Label_, State}|Rest] ->
            case Label_ =:= Label of
                true  -> Rest;
                false -> [{Fun, Label_, State}|remove(Label, Rest)]
            end
    end.