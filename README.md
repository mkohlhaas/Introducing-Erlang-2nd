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

fun ... end. % anonymous function
case ... of end.
if ... end.
receive ... end.
receive ... after ... end.

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

Processes:
> flush(). % see all messages in the mailbox and remove them
> self().  % own pid
> is_process_alive().
> self() ! some_message % sending a message to self using (!)

> h(erlang,spawn). % how to spawn a process
> Pid=spawn(bounce,report,[]).

> h(erlang,register). % how to register a process
> register(bounce,Pid).
> bounce ! hello. % sending message to registered process
> regs(). % see all registered processes
> unregister(bounce). % in case you don't need it any more
> BouncePid = whereis(bounce). % get pid of registered process

Monitor Erlang:
$ sudo xbps-install erlang-wx
> observer:start().

Link processes:
> h(erlang,spawn_link). % links are bidirectional: if one fails so does the other.

Trapping exits:
> h(erlang,process_flag).
> process_flag(trap_exit, true). % failure of a process will only lead to a message but not kill the process

Unlink a process:
> h(erlang,unlink).

Monitor a process:
> h(erlang,monitor).

Raise an exception/terminate a process:
> h(erlang,exit).

try ... [of]
  ...
catch
  error:Error -> {error, Error}
[after ...]
end.

Using catch in the shell:
> catch gen_server:call(drop, 60). % using catch just keeps the shell from ever receiving an exception, so the supervisor remains untouched

Raising Exceptions with throw:
> throw(my_exception).

try some:function(argument)
  catch
    error:Error -> {found, Error};
    throw:Exception -> {caught, Exception} % catching the throw
  end;


The error_logger module gives you three levels of reporting: info_msg, warning_msg, error_msg.
> m(error_logger).
> error_logger:info_msg("The value is ~p. ~n",[360]).

error_logger is used by SASL (System Architecture Support Libraries) to provide distributed error logging.

Debugging:
- compile code with 'debug_info':
- From the shell: $ erlc +debug_info module.erl
- From the Erlang shell: > c(module, [debug_info]).
- > debugger:start().

dbg module and erlang:trace* functions for tracing messages, function calls, etc.:
> h(dbg).
> dbg:tracer(). % start tracer
> h(dbg,p). % debug process
> dbg:p(Pid1,m). % debug process, trace messages

Storing Structured Data

1. Maps

> m(maps).
> h(maps,put).
> Planemos = #{ earth => 9.8, moon => 1.6, mars => 3.71 }.
> maps:get(moon, Planemos).
> #{earth := Gravity} = Planemos. % pattern matching works

2. Records

Record declaration (normally saved into a .hrl file):
-record(planemo, {name, gravity, diameter, distance_from_sun}).
-record(tower, {location, height=20, planemo=earth, name}). % with default values

Load record definitions:
> rr("records.hrl").

In source code:
-include("records.hrl").

Creating records:
> Tower1=#tower{}.
> Tower2=#tower{location="Grand Canyon"}.
> Tower2a=Tower2#tower{height=512}. % you always need to specify the record type (#tower) !

Reading records:
> Tower2#tower.planemo. % dot syntax
> #tower{location=L5, height=H5} = Tower2. % pattern matching

Using records in functions:
fall_velocity(#tower{} = T) -> fall_velocity(T#tower.planemo, T#tower.height). % syntax seems to be backwards
fall_velocity(#tower{planemo=Planemo, height=Distance}) -> fall_velocity(Planemo, Distance). % pattern matching the components
fall_velocity(#tower{planemo=Planemo, height=Distance} = T) -> ... % pattern matching on the whole and the components

3. ETS (Erlang Term Storage)



4. Mnesia

