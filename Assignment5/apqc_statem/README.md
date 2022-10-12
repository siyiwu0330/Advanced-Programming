AP QuickCheck State-Machine Library
===================================

This is a (partial) implementation of the `eqc_statem` library from
Quviq AB. It intended to have a subset of the API offered by
`eqc_statem`. Thus, any documentation for `eqc_statem` should be
usable as documentation for this library.

The library is build on top of Erlang QuickCheck Mini from Quviq AB,
which you need to install first.


Not for redistribution
----------------------

This library is only for use on the course Advanced Programming, and
should **not** be redistributed.


Getting started
---------------

1. Make sure you have Erlang QuickCheck Mini (or the full version)
   installed.

2. Move the files `apqc_statem.erl` and `apqc_statem.hrl` to the
   directory with your callback module for used with `apqc_statem`.

3. Add the following lines to your callback module:

    ~~~{.erlang}
    -include_lib("eqc/include/eqc.hrl").
    -include("apqc_statem.hrl").

    -behaviour(apqc_statem).
    -export([initial_state/0, command/1, precondition/2, postcondition/3, next_state/3]).
    ~~~

4. Implement you callback module.

5. Profit.
