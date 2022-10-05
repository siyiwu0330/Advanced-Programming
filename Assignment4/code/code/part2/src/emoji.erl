-module(emoji).

-export([start/1, new_shortcode/3, alias/3, delete/2, lookup/2,
         analytics/5, get_analytics/2, remove_analytics/3,
         stop/1]).

-type shortcode() :: string().
-type emoji() :: binary().
-type analytic_fun(State) :: fun((shortcode(), State) -> State).


start(_) -> not_implemented.

new_shortcode(_, _, _) -> not_implemented.

alias(_, _, _) -> not_implemented.

delete(_, _) -> not_implemented.

lookup(_, _) -> not_implemented.

analytics(_, _, _, _, _) -> not_implemented.

get_analytics(_, _) -> not_implemented.

remove_analytics(_, _, _) -> not_implemented.

stop(_) -> not_implemented.
