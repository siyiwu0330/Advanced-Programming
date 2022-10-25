-module(rps_coor).

% api called by rps.erl
-export([start/1, coor_move/2, coor_finish/1, coor_register_player/3]).



-export([callback_mode/0, init/1, ongoing/3, finished/3]).
-behaviour(gen_statem).



start(MaxRounds) -> 
    gen_statem:start(?MODULE,{[], nobody, [], 0, 0, 0, MaxRounds, none},[]).


coor_move(Coordinator, Choice) -> 
    gen_statem:call(Coordinator, {coor_move_trans, Choice, Coordinator}).
%gen_statem:call will wait for a process to use reply(From, Reply) in StateName_function 
%Only after reply(From, Reply) in StateName_function is used by a process, can the program here continue to execute.

coor_finish(Coordinator) -> % drain will call this
    gen_statem:cast(Coordinator, coor_finish_trans).

coor_register_player(BrokerRef, Coordinator, PPid)->
    gen_statem:cast(Coordinator, {coor_register_player_trans, BrokerRef, PPid}).




callback_mode() -> state_functions.

init({Participants, WaitP, ChoicesThisRound, RoundsWin1, RoundsWin2, TotalRounds, MaxRounds, BrokerRef}) ->
    {ok, ongoing, {Participants, WaitP, ChoicesThisRound, RoundsWin1, RoundsWin2, TotalRounds, MaxRounds, BrokerRef}}.

ongoing(cast,{coor_register_player_trans, BrokerRef, PPid}, State) ->
    case State of 
        {Participants, WaitP, ChoicesThisRound, RoundsWin1, RoundsWin2, TotalRounds, MaxRounds, _} -> 
            NewParticipants = [PPid|Participants],
            {keep_state, {NewParticipants, WaitP, ChoicesThisRound, RoundsWin1, RoundsWin2, TotalRounds, MaxRounds, BrokerRef}}
    end;
ongoing({call, From}, {coor_move_trans, Choice, Coordinator}, {Participants, WaitP, ChoicesThisRound, RoundsWin1, RoundsWin2, TotalRounds, MaxRounds, BrokerRef})->
    if
        ((TotalRounds == MaxRounds) or (RoundsWin1 > (MaxRounds/2)) or (RoundsWin2 > (MaxRounds/2))) ->
            {RealPid,_}=From,
            YourOrder = find_player_order(RealPid, Participants),
            case YourOrder of
                1 -> 
                    case WaitP of
                        nobody -> 
                            gen_statem:reply(From, {game_over, RoundsWin1, RoundsWin2}),
                            {keep_state, {Participants, From, ChoicesThisRound, RoundsWin1, RoundsWin2, TotalRounds, MaxRounds, BrokerRef}};
                        _ ->
                            gen_statem:reply(From, {game_over, RoundsWin1, RoundsWin2}),
                            rps:delete_coor(BrokerRef, Coordinator), 
                            rps:add_total_rounds(BrokerRef, TotalRounds),
                            {next_state, finished, {Participants, WaitP, ChoicesThisRound, RoundsWin1, RoundsWin2, TotalRounds, MaxRounds, BrokerRef}}
                    end;
                2 ->
                    case WaitP of
                        nobody -> 
                            gen_statem:reply(From, {game_over, RoundsWin2, RoundsWin1}),
                            {keep_state, {Participants, From, ChoicesThisRound, RoundsWin1, RoundsWin2, TotalRounds, MaxRounds, BrokerRef}};
                        _ ->
                            gen_statem:reply(From, {game_over, RoundsWin2, RoundsWin1}),
                            rps:delete_coor(BrokerRef, Coordinator), 
                            rps:add_total_rounds(BrokerRef, TotalRounds),
                            {next_state, finished, {Participants, WaitP, ChoicesThisRound, RoundsWin1, RoundsWin2, TotalRounds, MaxRounds, BrokerRef}}
                    end
            end; 
        TotalRounds < MaxRounds ->
            case WaitP of
                nobody -> %you are the first one to give the choice
                    NewChoicesThisRound = [Choice|ChoicesThisRound],
                    {keep_state,  {Participants, From, NewChoicesThisRound, RoundsWin1, RoundsWin2, TotalRounds, MaxRounds, BrokerRef}};
                _ -> %you are the second one to give the choice, so you should update the result
                    {RealPid,_}=From,
                    YourOrder = find_player_order(RealPid, Participants), %get player order
                    case YourOrder of
                        1 ->
                            NewChoicesThisRound = [Choice|ChoicesThisRound],
                            Winner = judge_winner(NewChoicesThisRound),
                            case Winner of
                                1 ->
                                    gen_statem:reply(From, win),
                                    gen_statem:reply(WaitP, {loss, Choice}),
                                    {keep_state,{Participants, nobody, [], RoundsWin1+1, RoundsWin2, TotalRounds+1, MaxRounds, BrokerRef}};
                                2 ->
                                    OpChoice = lists:nth(2,NewChoicesThisRound),
                                    gen_statem:reply(From, {loss, OpChoice}),
                                    gen_statem:reply(WaitP, win),
                                    {keep_state,{Participants, nobody, [], RoundsWin1, RoundsWin2+1, TotalRounds+1, MaxRounds, BrokerRef}};
                                3 ->
                                    gen_statem:reply(From, tie),
                                    gen_statem:reply(WaitP, tie),
                                    {keep_state,{Participants, nobody, [], RoundsWin1, RoundsWin2, TotalRounds+1, MaxRounds, BrokerRef}}
                            end;
                        2 ->
                            NewChoicesThisRound = [Choice|ChoicesThisRound],
                            Winner = judge_winner(NewChoicesThisRound),
                            case Winner of
                                1 ->
                                    gen_statem:reply(From, win),
                                    gen_statem:reply(WaitP, {loss, Choice}),
                                    {keep_state,{Participants, nobody, [], RoundsWin1, RoundsWin2+1, TotalRounds+1, MaxRounds, BrokerRef}};
                                2 ->
                                    OpChoice = lists:nth(2,NewChoicesThisRound),
                                    gen_statem:reply(From, {loss, OpChoice}),
                                    gen_statem:reply(WaitP, win),
                                    {keep_state,{Participants, nobody, [], RoundsWin1+1, RoundsWin2, TotalRounds+1, MaxRounds, BrokerRef}};
                                3 ->
                                    gen_statem:reply(From, tie),
                                    gen_statem:reply(WaitP, tie),
                                    {keep_state,{Participants, nobody, [], RoundsWin1, RoundsWin2, TotalRounds+1, MaxRounds, BrokerRef}}
                            end
                            
                    end
        
                    
            end
            
    end;
ongoing(cast, coor_finish_trans, State) ->
    {next_state, finished, State}.

finished({call, From},{coor_move_trans, _, _}, State) ->
    gen_statem:reply(From, server_stopping),
    {next_state, finished, State}.

%some helper functions
judge_winner(GestureList) ->
    case GestureList of
        [rock,paper] -> 2;
        [rock,scissors] -> 1;
        [rock,rock] -> 3;
        [paper,rock] -> 1;
        [paper,paper] -> 3;
        [paper,scissors] -> 2;
        [scissors,rock] -> 2;
        [scissors,paper] -> 1;
        [scissors,scissors] -> 3;
        [rock,_] -> 1;
        [paper,_] -> 1;
        [scissors,_] -> 1;
        [_,rock] -> 2;
        [_,paper] -> 2;
        [_,scissors] -> 2;
        [_,_] -> 3
    end.

find_player_order(PPid, Participants) ->
    Player1 = lists:nth(1, Participants),
    Player2 = lists:nth(2, Participants),
    if
        PPid == Player1 -> 1;
        PPid == Player2 -> 2
    end.