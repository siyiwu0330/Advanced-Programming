-module(test_emoji).

-export([test_all/0]).

% We'll use EUnit
-include_lib("eunit/include/eunit.hrl").

test_all() -> eunit:test(testsuite(), [verbose]).

testsuite() ->
    [ {"Basic behaviour", spawn,
       [ test_start_server()
       , test_shortcode_smiley()
       ]
      }
    ].

test_start_server() ->
    {"We can call start/1 and it does not crash",
     fun () ->
       ?assertMatch({ok, _}, emoji:start([]))
     end }.

test_shortcode_smiley() ->
    {"Register new shortcode",
     fun () ->
       {ok, S} = emoji:start([]),
       ?assertEqual(ok, emoji:new_shortcode(S, "smiley",
                                            <<240,159,152,131>>))
     end }.
