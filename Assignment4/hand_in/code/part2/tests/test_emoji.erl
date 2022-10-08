-module(test_emoji).

-export([test_all/0]).

% We'll use EUnit
-include_lib("eunit/include/eunit.hrl").

test_all() -> eunit:test(testsuite(), [verbose]).

testsuite() ->
    [ {"Basic behaviour", spawn,
       [ test_start_server()
       , test_shortcode_smiley()
       , test_shortcode_smiley_lookup()
       ,test_smiley_alias_and_lookup()
       ,test_smiley_alias_and_error()
       ,test_smiley_alias_and_ask_list()
       ,test_smiley_alias_and_delete()
       ]
      },
      {"Analytics", spawn,
       [ test_register_analytics()
       ,test_register_function()
       ,test_2register_function()
       ,test_2register_function_with_alias()
       ,test_2register_function_with_multiple_alias()
       ,test_register_remove_with_alias()
       ]
      },
      {"Scale", spawn,
      [ test_scale_small()
       ,test_scale_medium()
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
test_shortcode_smiley_lookup() ->
      {"Register and lookup new shortcode",
       fun () ->
         {ok, S} = emoji:start([]),
         emoji:new_shortcode(S, "smiley",<<240,159,152,131>>),
         ?assertMatch({ok, <<240,159,152,131>>}, emoji:lookup(S, "smiley"))
       end }.

test_smiley_alias_and_lookup() ->
{"Register and lookup new shortcode with alias",
  fun () ->
    {ok, S} = emoji:start([]),
    emoji:new_shortcode(S, "smiley",<<240,159,152,131>>),
    emoji:alias(S, "smiley", "smiley1"),
    ?assertMatch({ok, <<240,159,152,131>>}, emoji:lookup(S, "smiley1"))
  end }.

test_smiley_alias_and_error() ->
  {"make an alias error",
    fun () ->
      {ok, S} = emoji:start([]),
      emoji:new_shortcode(S, "smiley",<<240,159,152,131>>),
      ?assertMatch({error, _}, emoji:alias(S, "smiley", "smiley"))
    end }.

test_smiley_alias_and_ask_list() ->
  {"test the alias registration",
    fun () ->
      {ok, S} = emoji:start([]),
      emoji:new_shortcode(S, "smiley",<<240,159,152,131>>),
      emoji:alias(S, "smiley", "smiley1"),
      ?assertEqual({ok,<<240,159,152,131>>}, emoji:lookup(S, "smiley1"))
    end }.

test_smiley_alias_and_delete() ->
  {"delete shrotcode and its alias",
    fun () ->
      {ok, S} = emoji:start([]),
      emoji:new_shortcode(S, "smiley",<<240,159,152,131>>),
      emoji:alias(S, "smiley", "smiley1"),
      emoji:delete(S, "smiley"),
      ?assertEqual(no_emoji, emoji:lookup(S, "smiley1"))
    end }.

test_register_analytics() ->
  {"Register analytics function for a new shortcode",
    fun () ->
      {ok, S} = emoji:start([]),
      emoji:new_shortcode(S, "smiley",<<240,159,152,131>>),
      Hit = fun(_, N) -> N+1 end,
      emoji:analytics(S, "smiley",  Hit, "Hit", 0),
      ?assertEqual({ok, [{"Hit", 0}]}, emoji:get_analytics(S, "smiley"))
    end }.

test_register_function() ->
  {"Register analytics function and test it by lookup",
    fun () ->
      {ok, S} = emoji:start([]),
      emoji:new_shortcode(S, "smiley",<<240,159,152,131>>),
      Hit = fun(_, N) -> N+1 end,
      emoji:analytics(S, "smiley",  Hit, "Hit", 0),
      emoji:lookup(S,"smiley"),
      ?assertEqual({ok, [{"Hit", 1}]}, emoji:get_analytics(S, "smiley"))
    end }.
    
test_2register_function() ->
  {"Register 2 analytics functions and test them by lookup",
    fun () ->
      {ok, S} = emoji:start([]),
      emoji:new_shortcode(S, "smiley",<<240,159,152,131>>),
      Hit = fun(_, N) -> N+1 end,
      Last = fun (S1, _) -> S1 end,
      emoji:analytics(S, "smiley",  Hit, "Hit", 0),
      emoji:analytics(S, "smiley",  Last, "Last", none),
      emoji:lookup(S,"smiley"),
      ?assertEqual({ok, [{"Last", "smiley"} ,{"Hit", 1}]}, emoji:get_analytics(S, "smiley"))
    end }.

test_2register_function_with_alias() ->
  {"Register 2 analytics functions for a short code with alias and test them by lookup",
    fun () ->
      {ok, S} = emoji:start([]),
      emoji:new_shortcode(S, "smiley",<<240,159,152,131>>),
      emoji:alias(S, "smiley", "smiley1"),
      Hit = fun(_, N) -> N+1 end,
      Last = fun (S1, _) -> S1 end,
      emoji:analytics(S, "smiley1",  Hit, "Hit", 0),
      emoji:analytics(S, "smiley1",  Last, "Last", none),
      emoji:lookup(S,"smiley1"),
      ?assertEqual({ok, [{"Last", "smiley1"} ,{"Hit", 1}]}, emoji:get_analytics(S, "smiley1"))
    end }.

test_2register_function_with_multiple_alias() ->
  {"Register 2 analytics functions for a short code with multiple aliases and test them by lookup",
    fun () ->
      {ok, S} = emoji:start([]),
      emoji:new_shortcode(S, "smiley",<<240,159,152,131>>),
      emoji:alias(S, "smiley", "smiley1"),
      emoji:alias(S, "smiley", "smiley2"),
      Hit = fun(_, N) -> N+1 end,
      Last = fun (S1, _) -> S1 end,
      emoji:analytics(S, "smiley1",  Hit, "Hit", 0),
      emoji:analytics(S, "smiley1",  Last, "Last", none),
      emoji:lookup(S,"smiley1"),
      ?assertEqual({ok, [{"Last", "smiley1"} ,{"Hit", 1}]}, emoji:get_analytics(S, "smiley2"))
    end }.
    
test_register_remove_with_alias()->
  {"Removing registered analytics function with multiple aliases",
  fun () ->
    {ok, S} = emoji:start([]),
    emoji:new_shortcode(S, "smiley",<<240,159,152,131>>),
    emoji:alias(S, "smiley", "smiley1"),
    emoji:alias(S, "smiley", "smiley2"),
    Hit = fun(_, N) -> N+1 end,
    Last = fun (S1, _) -> S1 end,
    emoji:analytics(S, "smiley1",  Hit, "Hit", 0),
    emoji:analytics(S, "smiley1",  Last, "Last", none),
    emoji:remove_analytics(S, "smiley", "Hit"),
    ?assertEqual({ok, [{"Last", none}]}, emoji:get_analytics(S, "smiley"))
  end }.

test_scale_small() ->
  {"initial with small amount of emoji and lookup",
  fun () ->
    InitialList = someemoji:small(),
    {ok, S} = emoji:start(InitialList),
    ?assertEqual({ok,<<"👢"/utf8>>}, emoji:lookup(S,"boot"))
  end }.

test_scale_medium() ->
  {"initial with small amount of emoji and lookup",
  fun () ->
    InitialList = someemoji:medium(),
    {ok, S} = emoji:start(InitialList),
    ?assertEqual({ok,<<240,159,145,168,240,159,143,190,226,128,141,240,159,146,187>>}, 
    emoji:lookup(S,"man technologist: medium-dark skin tone"))
  end }.

% test_after_stop()->
%   {"test after stop the server",
%   fun () ->
%     InitialList = someemoji:small(),
%     {ok, S} = emoji:start(InitialList),
%     emoji:stop(S),
%     ?assertMatch({error, _}, emoji:lookup(S,"boot"))
%   end }.