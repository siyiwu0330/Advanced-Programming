-module(paper_bot).
-export([queue_up_and_play/1]).

queue_up_and_play(Broker) ->
    {ok, _Other, Coor} = rps:queue_up(Broker, "Paper bot(amy)", 3),
    paper_to_game_over(Coor).

paper_to_game_over(Coor) ->
    case rps:move(Coor, paper) of
        {game_over, Me, SomeLoser} ->
            {ok, Me, SomeLoser};
        server_stopping ->
            server_stopping;
        _ -> paper_to_game_over(Coor)
    end.
