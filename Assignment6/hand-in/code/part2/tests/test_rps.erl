-module(test_rps).
-export([test_all/0]).

%% Maybe you want to use eunit
-include_lib("eunit/include/eunit.hrl").


test_all() ->
    eunit:test(
      [
       start_broker(),
       queue_up(),
       queue_up_not_match(),
       queue_up_and_normal_play1(),
       queue_up_and_strange_play1(),
       queue_up_and_strange_play1_to_end(),
       statistics_after_play3(),
       statistics_after_play3_bots(),
       try_move_after_drain(),
       try_queue_after_drain(),
       queue_get_drain_message()
      ], [verbose]).


start_broker() ->
  {"Start a broker, and nothing else",
    fun() ->
      % one kind of assertMatch(at the end of "fun()"")
      ?assertMatch({ok, _}, rps:start())  
    end}.

queue_up() ->
  {"Start a broker, and queue",
    fun() ->
      {ok, S}=rps:start(),
      Pid = self(),
      TestProcess1 = fun(PidT, ST) ->  
        Res = rps:queue_up(ST,"Bob",1),  
        % Me=self(),
        PidT ! {p1,Res}  end,
      TestProcess2 = fun(PidT, ST) ->  
        Res = rps:queue_up(ST,"Amy",1),  
        %Me=self(),
        PidT ! {p2,Res}  end,
      spawn(fun() -> TestProcess1(Pid, S) end),
      spawn(fun() -> TestProcess2(Pid, S) end),
      receive   
        {p1, Res} -> 
          % ?assertMatch (?assertEqual) can also be written in this way
          ?assertMatch({ok, "Amy", _}, Res),
          receive
            {p2, Res1} ->
              ?assertMatch({ok, "Bob", _}, Res1)
          end  
      end
    end}.

queue_up_not_match() ->
  {"Start a broker, and queue, but not match",
    fun() ->
      {ok, S}=rps:start(),
      TestProcess1 = fun(ST) ->  
        rps:queue_up(ST,"Bob",3)
        end,
      TestProcess2 = fun(ST) ->  
        rps:queue_up(ST,"Amy",1)
        end,
      spawn(fun() -> TestProcess1(S) end),
      spawn(fun() -> TestProcess2(S) end),
      timer:sleep(700),
      Res=rps:statistics(S),
      ?assertEqual({ok, 0, 2, 0}, Res)
    end}.


queue_up_and_normal_play1() ->
  {"Start a broker, queue, and play a round normally",
    fun() ->
      {ok, S}=rps:start(),
      Pid = self(),
      TestProcess1 = fun(PidT, ST) ->  
        {ok, _, Coordinator} = rps:queue_up(ST,"Bob",1),
        Res=rps:move(Coordinator, rock),
        %Me=self(),
        PidT ! {p1,Res}  end,
      TestProcess2 = fun(PidT, ST) -> 
        {ok, _, Coordinator} = rps:queue_up(ST,"Amy",1),  
        Res=rps:move(Coordinator, paper),
        %Me=self(),
        PidT ! {p2,Res}  end,
      spawn(fun() -> TestProcess1(Pid, S) end), 
      spawn(fun() -> TestProcess2(Pid, S) end),
      receive   
        {p1, Res} -> 
          ?assertEqual({loss, paper}, Res),
          receive
            {p2, Res1} ->
              ?assertEqual(win, Res1)
          end
      end
    end}.

queue_up_and_strange_play1() ->
  {"Start a broker, queue, and play a round strangely",
    fun() ->
      {ok, S}=rps:start(),
      Pid = self(),
      TestProcess1 = fun(PidT, ST) ->  
        {ok, _, Coordinator} = rps:queue_up(ST,"Bob",1),
        Res=rps:move(Coordinator, laser),
        %Me=self(),
        PidT ! {p1,Res}  end,
      TestProcess2 = fun(PidT, ST) -> 
        {ok, _, Coordinator} = rps:queue_up(ST,"Amy",1),  
        Res=rps:move(Coordinator, paper),
        %Me=self(),
        PidT ! {p2,Res}  end,
      spawn(fun() -> TestProcess1(Pid, S) end), 
      spawn(fun() -> TestProcess2(Pid, S) end),
      receive   
        {p1, Res} -> 
          ?assertEqual({loss, paper}, Res),
          receive
            {p2, Res1} ->
              ?assertEqual(win, Res1)
          end 
      end
    end}.

queue_up_and_strange_play1_to_end() ->
  {"Start a broker, queue, and play a round strangely and see the final result",
    fun() ->
      {ok, S}=rps:start(),
      Pid = self(),
      TestProcess1 = fun(PidT, ST) ->  
        {ok, _, Coordinator} = rps:queue_up(ST,"Bob",1),
        rps:move(Coordinator, laser),
        Res= rps:move(Coordinator, thumbs_up),
        %Me=self(),
        PidT ! {p1,Res}  end,
      TestProcess2 = fun(PidT, ST) -> 
        {ok, _, Coordinator} = rps:queue_up(ST,"Amy",1),  
        rps:move(Coordinator, paper),
        Res=rps:move(Coordinator, rock),
        %Me=self(),
        PidT ! {p2,Res}  end,
      spawn(fun() -> TestProcess1(Pid, S) end), 
      spawn(fun() -> TestProcess2(Pid, S) end),
      receive   
        {p1, Res} -> 
          ?assertEqual({game_over, 0, 1}, Res),
          receive
            {p2, Res1} ->
              ?assertEqual({game_over, 1, 0}, Res1)
          end 
      end
    end}.

statistics_after_play3() ->
  {"Start a broker, queue and play 3 rounds and see the result",
    fun() ->
      {ok, S}=rps:start(),
      TestProcess1 = fun(ST) ->  
        {ok, _, Coordinator} = rps:queue_up(ST,"Bob",3),
        rps:move(Coordinator, laser),
        rps:move(Coordinator, rock),
        rps:move(Coordinator, scissors)
        end,
      TestProcess2 = fun(ST) -> 
        {ok, _, Coordinator} = rps:queue_up(ST,"Amy",3),  
        rps:move(Coordinator, paper),
        rps:move(Coordinator, paper),
        rps:move(Coordinator, scissors)
        end,
      spawn(fun() -> TestProcess1(S) end), 
      spawn(fun() -> TestProcess2(S) end),
      timer:sleep(1000),
      Res=rps:statistics(S),
      ?assertEqual({ok, 2, 0, 0},Res)
    end}.

statistics_after_play3_bots() ->
  {"Start a broker, play 3 rounds by bots and see the result",
    fun() ->
      {ok, S}=rps:start(),
      spawn(rock_bot, queue_up_and_play, [S]),
      spawn(paper_bot, queue_up_and_play, [S]),
      timer:sleep(700),
      Res=rps:statistics(S),
      ?assertEqual({ok, 2, 0, 0},Res)
    end}.


try_move_after_drain() ->
  {"Players who try to move after the server is stopped",
    fun() ->
      {ok, S}=rps:start(),
      Pid = self(),
      TestProcess1 = fun(PidT,ST) ->  
        {ok, _, Coordinator}=rps:queue_up(ST,"Bob",1),
        timer:sleep(1000),
        Res=rps:move(Coordinator, rock),
        PidT ! {p1, Res}
        end,
      TestProcess2 = fun(PidT,ST) ->  
        {ok, _, Coordinator}=rps:queue_up(ST,"Amy",1),
        timer:sleep(1000),
        Res=rps:move(Coordinator, rock),
        PidT ! {p2, Res}
        end,
      spawn(fun() -> TestProcess1(Pid,S) end),
      spawn(fun() -> TestProcess2(Pid,S) end),
      timer:sleep(500),
      rps:drain(S,none,none),
      receive   
        {p1, Res} -> 
          ?assertEqual(server_stopping, Res),
          receive
            {p2, Res1} ->
              ?assertEqual(server_stopping, Res1)
          end 
      end
    end}.


try_queue_after_drain() ->    
  {"Players who try to queue up at the broker after the server is stopped",
    fun() ->
      {ok, S}=rps:start(),
      Pid = self(),
      TestProcess1 = fun(PidT,ST) ->  
        Res=rps:queue_up(ST,"Bob",3),
        PidT ! {p1, Res}
        end,
      TestProcess2 = fun(PidT,ST) ->  
        Res=rps:queue_up(ST,"Amy",1),
        PidT ! {p2, Res}
        end,
      rps:drain(S,none,none),
      spawn(fun() -> TestProcess1(Pid,S) end),
      spawn(fun() -> TestProcess2(Pid,S) end),
      receive   
        {p1, Res} -> 
          ?assertEqual(error_server_stopping, Res),
          receive
            {p2, Res1} ->
              ?assertEqual(error_server_stopping, Res1)
          end 
      end
    end}.




queue_get_drain_message() ->
  {"Players, who are queued up at the broker, get notified when the server is stopping",
    fun() ->
      {ok, S}=rps:start(),
      Pid = self(),
      TestProcess1 = fun(PidT,ST) ->  
        Res=rps:queue_up(ST,"Bob",3),
        PidT ! {p1, Res}
        end,
      TestProcess2 = fun(PidT,ST) ->  
        Res=rps:queue_up(ST,"Amy",1),
        PidT ! {p2, Res}
        end,
      spawn(fun() -> TestProcess1(Pid,S) end),
      spawn(fun() -> TestProcess2(Pid,S) end),
      rps:drain(S,none,none),
      receive   
        {p1, Res} -> 
          ?assertEqual(error_server_stopping, Res),
          receive
            {p2, Res1} ->
              ?assertEqual(error_server_stopping, Res1)
          end 
      end
    end}.