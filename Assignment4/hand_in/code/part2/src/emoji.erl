-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1]).

-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).


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


loop(E) ->
    ShortList = 
        lists:map(
            fun(Emoji) ->
                case Emoji of
                    {Short, _, _, _} -> Short
                end
            end, E),
    receive
        {From, {new_shortcode, Short, Emo}} ->
            case lists:member(Short, ShortList) of
                true  ->
                    From!{self(), {error, "emoji already exists"}},
                    loop(E);
                false ->
                    From!{self(), ok},
                    loop([{Short, Emo, Short, []}|E])
            end;
        {From, {alias, Short1, Short2}} ->
            case lists:member(Short1, ShortList) of
                false ->
                    From!{self(), {error, "emoji_1 does not exist"}},
                    loop(E);
                true  ->
                    case lists:member(Short2, ShortList) of
                        true  ->
                            From!{self(), {error, "emoji_2 already exists"}},
                            loop(E);
                        false ->
                            From!{self(), ok},
                            loop([{Short2, getEmo(Short1, E), realShort(Short1, E), []}|E])
            end;
        {From, {delete, Short}} ->
            case lists:member(Short, ShortList) of
                false ->
                    From!{self, {error, "emoji does not exist"}},
                    loop(E);
                true  ->
                    From!{self(), ok},
                    loop(deleteAll(Short, E))
            end;
        {From, {lookup, Short}} ->
            case lists:member(Short, ShortList) of
                false ->
                    From!{self(), no_emoji},
                    loop(E);
                true  ->
                    From!{self(), {ok, getEmo(Short, E)}},
                    loop(E)
            end;
        {From, {analytics, Short, Fun, Label, Init}} ->
            case lists:member(Short, ShortList) of
                false ->
                    From!{self(), {error, "emoji does not exist"}},
                    loop(E);
                true  ->
                    From!{self(), ok},
                    loop(bindFun(Short, Fun, Label, Init, E))
            end;
        {From, {get_analytics, Short}} ->
            case lists:member(Short, ShortList) of
                false ->
                    From!{self(), {error, "emoji does not exist"}},
                    loop(E);
                true  ->
                    From!{self(), {ok, getStat(Short, E)}},
                    loop(E)
            end;
        {From, {remove_analytics, Short, Label}} ->
            case lists:member(Short, ShortList) of
                false ->
                    From!{self(), {error, "emoji does not exist"}},
                    loop(E);
                true  ->
                    From!{self(), ok},
                    loop(removeFun(Short, Label, E))
            end;
        {From, stop} -> From!{self(), ok};
        {From, _} ->
            From!{self(), "syntax error"},
            loop(E)
    end
end.



duplicate(ShortList)->
    case ShortList of
        []          -> false;
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

realShort(Short, E) ->
    case E of
        [] -> noting;
        [{Short_, _, RealShort, _}|Rest] ->
            case Short_ =:= Short of
                true  -> RealShort;
                false -> realShort(Short, Rest)
            end
    end.

getEmo(Short, E) ->
    case E of
        [] -> noting;
        [{Short_, Emo, _, _}|Rest] ->
            case Short_ =:= Short of
                true  -> Emo;
                false -> getEmo(Short, Rest)
            end
    end.

deleteAll(Short, E) ->
    case E of
        [] -> [];
        [{Short_, Emo, RealShort, FunList}|Rest] ->
            case RealShort =:= realShort(Short, E) of
                true  -> [deleteAll(Short, Rest)];
                false -> [{Short_, Emo, RealShort, FunList}|deleteAll(Short, Rest)]
            end
    end.

bindFun(Short, Fun, Label, Init, E) ->
    case E of
        [] -> [];
        [{Short_, Emo, RealShort, FunList}|Rest] ->
            case Short_ =:= Short of
                false -> [{Short_, Emo, RealShort, FunList}|bindFun(Short, Fun, Label, Init, Rest)];
                true  ->
                    case RealShort =:= realShort(Short, E) of
                        false -> bindFun(RealShort, Fun, Label, Init, E);
                        true  -> [{Short_, Emo, RealShort, [{Fun, Label, Init}|FunList]}]
                    end
            end
    end.

getStat(Short, E) ->
    case E of
        [] -> nothing;
        [{Short_, _, RealShort, FunList}]