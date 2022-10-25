-module(rps).
-export([start/0, queue_up/3, move/2, statistics/1, drain/3, get_server_state/1, delete_coor/2, add_total_rounds/2]).




-export([init/1, handle_call/3, handle_cast/2]).
-behaviour(gen_server).


start() -> gen_server:start(?MODULE,{active, [], [0], []},[]).

queue_up(BrokerRef, Name, Rounds) -> 
    ServerState = get_server_state(BrokerRef),
    Me = self(),
    if 
        ServerState =:= drain -> error_server_stopping;
        Rounds < 0 -> {error, rounds_should_be_non_negative};
        Rounds >= 0 -> 
            {OtherPlayer, Coordinator} = gen_server:call(BrokerRef,{queue_up_trans, Name, Rounds}),
            rps_coor:coor_register_player(BrokerRef, Coordinator, Me),
            {ok, OtherPlayer, Coordinator}
    end.

move(Coordinator, Choice) -> 
    rps_coor:coor_move(Coordinator, Choice).

statistics(BrokerRef) -> gen_server:call(BrokerRef, statistics_trans).

drain(BrokerRef, Pid, Msg) ->  
    if
        Pid =/= none -> 
            Pid ! Msg,
            gen_server:cast(BrokerRef, drain_trans);
        Pid =:= none ->
            gen_server:cast(BrokerRef, drain_trans)
    end.

init({StateFlag, Queue, FinishedCoorsRounds, OngoingCoors}) ->
    {ok, {StateFlag, Queue, FinishedCoorsRounds, OngoingCoors}}.

handle_call({queue_up_trans, Name, Rounds}, _From, State) ->
    case State of
        {active, Queue, FinishedCoorsRounds, OngoingCoors} -> 
            FindMatchResult = lists:keyfind(Rounds, 2, Queue),
            case FindMatchResult of
                false -> 
                    NewQueue = [{Name, Rounds, _From}]++Queue,
                    {noreply, {active, NewQueue, FinishedCoorsRounds, OngoingCoors}};
                {OtherPlayer, _, _OpFrom} -> 
                    {ok, Coordinator} = rps_coor:start(Rounds), % Rounds is also MaxRounds in "rps_coor.erl"
                    gen_server:reply(_OpFrom, {Name, Coordinator}),
                    NewQueue = lists:delete(FindMatchResult, Queue),
                    NewOngoingCoors = [Coordinator|OngoingCoors],
                    {reply, {OtherPlayer, Coordinator}, {active, NewQueue, FinishedCoorsRounds, NewOngoingCoors}} 
            end
    end;
handle_call(statistics_trans, _From, State) ->
    case State of
        {active, Queue, FinishedCoorsRounds, OngoingCoors} -> 
            LongestGame = lists:max(FinishedCoorsRounds),
            InQueueNum = length(Queue),
            OngoingCoorsNum = length(OngoingCoors),
            {reply, {ok, LongestGame, InQueueNum, OngoingCoorsNum}, State}
    end;
handle_call(get_server_state_trans, _From, State) ->
    case State of
        {active, _, _, _} -> {reply, active, State};
        {drain, _, _, _} -> {reply, drain, State}
    end.



handle_cast(drain_trans, State) ->
    case State of
        {active, Queue, FinishedCoorsRounds, OngoingCoors} -> 
            Fun = fun(Coordinator) -> rps_coor:coor_finish(Coordinator) end,
            lists:foreach(Fun, OngoingCoors),
            FunStop = fun (ElemTuple)->
                case ElemTuple of
                    {_, _, Pid} -> gen_server:reply(Pid, error_server_stopping)
                end end,
            lists:foreach(FunStop, Queue),
            {noreply, {drain, Queue, FinishedCoorsRounds, OngoingCoors}};
        _ -> {noreply, State}
    end;
handle_cast({delete_coor_trans, Coordinator},State) -> 
    case State of
        {StateFlag, Queue, FinishedCoorsRounds, OngoingCoors} -> 
            NewOngoingCoors = lists:delete(Coordinator, OngoingCoors),
            {noreply, {StateFlag, Queue, FinishedCoorsRounds, NewOngoingCoors}}
    end;
handle_cast({add_total_rounds_trans, TotalRounds},State) -> 
    case State of
        {StateFlag, Queue, FinishedCoorsRounds, OngoingCoors} -> 
            NewFinishedCoorsRounds = [TotalRounds|FinishedCoorsRounds],
            {noreply, {StateFlag, Queue, NewFinishedCoorsRounds, OngoingCoors}}
    end.


%some api functions

get_server_state(BrokerRef) -> 
    gen_server:call(BrokerRef, get_server_state_trans).

delete_coor(BrokerRef, Coordinator) ->
    gen_server:cast(BrokerRef, {delete_coor_trans, Coordinator}).

add_total_rounds(BrokerRef, TotalRounds) ->
    gen_server:cast(BrokerRef, {add_total_rounds_trans, TotalRounds}).
