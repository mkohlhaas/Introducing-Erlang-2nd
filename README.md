# Introducing Erlang, 2nd Edition

Code samples from Introducing Erlang, second edition

Erlang shell:
$ erl
Help in Erlang shell:
> help(). % look what's available. Important!
> h(gen_server).
> h(supervisor).

Info about modules:
> m(erlang). % built-in functions
> m(lists).  % list functions

> [1,2,4,8,16,32]. % a simple list

hd and tl functions are in the erlang module.
> h(erlang, hd).
> h(erlang, tl).

; acts like OR
, acts like AND

Help from the UNIX shell:
$ erl -man erlang
$ erl -man lists
$ erl -man gen_server

$ erlc --help
$ erlc -o ./ebin ./src/*.erl

> flush(). % see all messages in the mailbox and remove them
> self().  % own pid
> is_process_alive().
> self() ! some_message % sending a message to self using (!)

fun ... end. % anonymous function
case ... of end.
if ... end.
receive ... end.

> q(). % shuts down the whole erlang system! Not only the shell!

Use Ctrl-C Ctrl-C to end the shell but not the Erlang system.

Base#Value notation
> 2#1010111.
> 16#cafe.

Variables are uppercase, atoms lowercase.

> b(). % show bindings
> f(). % forget bindings

Directives:
-module
-export
-import

-module(drop).
-export([fall_velocity/1, mps_to_mph/1, mps_to_kph/1]).
-import(convert, [mps_to_mph/1]).

Refer to a function with fun:
fun drop:fall_velocity/1.

Check out edoc module for documenting Erlang code.

Guards with when:
absolute_value(Number) when Number < 0 -> -Number;

Don't care variables with _
fall_velocity(_, Distance) -> math:sqrt(2 * 9.8 * Distance).

Similar with _name: the variable is bound, but compiler does not complain if it's not used.

Tuples with {}:
> {earth, 20}.

Input/output with io module:
> m(io).

> io:format("Look out below! ~w is too high.~n", [Distance]) ;

String functions:
> m(string).

String concatenation:
> string:concat("erl", "ang").

Or simply because strings are just lists:
> "erl" ++ "ang". % list concatenation

List comprehensions:
> [Value * Value || Value <- [1,4,16,64,256,1024]].
