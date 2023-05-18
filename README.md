# Introducing Erlang, 2nd Edition

Code samples from Introducing Erlang, second edition

Erlang shell:
$ erl
Help in Erlang shell:
> help(). % look what's available. Important!

h(Mod)            -- help about module
h(Mod,Func)       -- help about function in module
h(Mod,Func,Arity) -- help about function with arity in module

hcb(Mod)          -- help about a module's callbacks
hcb(Mod,CB)       -- help about callback in module
hcb(Mod,CB,Arity) -- help about callback with arity in module

> m(gen_server). % what's available
> h(gen_server). % module help
> m(supervisor).
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

3. (D)ETS ((Disk) Erlang Term Storage)

DETS and ETS tables use records underneath and are basis for the Mnesia database.
DETS have a 2GB limit.

Collection    | Behavior
--------------+--------------------------------------------------------------------------------------------------------
Set (default) | one entry with a given key
Ordered Set   | same as set plus traversal order based on the keys
Bag           | more entries for a given key, multiple entries with identical values are combined into a single entry
Duplicate Bag | same as bag but you can have multiple entries with identical values
--------------+--------------------------------------------------------------------------------------------------------

> m(ets).
> h(ets).
> h(ets,new). % most important options are named_table and keypos
              % returns a table which can be referenced in code

named_table allows to reference the table just by its name without using the returned table reference.
By default ETS treats the first value in a tuple as the key. But records use the first value in a tuple to identify its kind.
So this is not very useful as the record key.
keypos allows you to specify which record value should be the key.

PlanemoTable = ets:new(planemos,[ named_table, {keypos, #planemo.name} ]) % see record definition for planemo above, using then name as key

Access              | Behavior
--------------------+---------------------------------------------------------------------------------------------
protected (default) | The owner process can read and write to the table. Other processes can only read the table.
public              | Any process can read or write to the table.
private             | Only the owner process can read or write to the table.
--------------------+---------------------------------------------------------------------------------------------

> h(ets,info).
> h(ets,insert).

ets:insert(planemos, #planemo{ name=mercury, gravity=3.7, diameter=4878, distance_from_sun=57.9 }),

> h(ets,tab2list). % returns a list of all objects in table
> ets:tab2list(planemos). % really helpful in the shell

You can also use the Table Viewer tab in the observer.
> observer:start().

> h(ets,i). % displays information about all ETS tables and browses tables on a terminal

> h(ets,lookup). % returns a list of all objects with a specific key
> ets:lookup(planemos, mercury).

> h(ets,give_away). % makes a process the new owner of the table, also check the heir option

More complex queries are possible with:
> h(ets,fun2ms).
> h(ets,match).
> h(ets,select).
> h(ets,first).
> h(ets,next).
> h(ets,last).

Delete entries:
> h(ets,delete).

4. Mnesia

Useful for storing data across nodes.

> m(mnesia).
> h(mnesia).
> h(mnesia,create_schema). % creates a new database on disc

> mnesia:create_schema([node()]).

> h(erlang,node). %returns the name of the local or remote node

> node().       % name of the local node
> node(self()). % name of the local node

$ erl -mnesia dir "path" # store mnesia database in a certain directory

> mnesia:start(). % start Mnesia database
> mnesia:stop().  % stop Mnesia database

https://www.erlang.org/doc/reference_manual/records.html#internal-representation-of-records
To each module using records, a pseudo function is added during compilation to obtain information about records:
record_info(fields, Record) -> [Field]
record_info(size, Record) -> Size

> h(mnesia,create_table).

mnesia:create_table(planemo, [{attributes, record_info(fields, planemo)}]). % ram_copies, disc_copies, disc_only_copies can be defined for different nodes!

> h(mnesia,write). % writes a record to the table
> mnesia:write(#planemo{ name=mercury, gravity=3.7, diameter=4878, distance_from_sun=57.9 }).

> h(mnesia,transaction). % executes a functional object as a transaction

F = fun() -> mnesia:write(#planemo{ name=mercury, gravity=3.7, diameter=4878, distance_from_sun=57.9 }) end.
mnesia:transaction(F).

> h(mnesia,read).
> h(mnesia,delete).
> h(mnesia,table_info).

> mnesia:table_info(planemo,all).

You can look at the contents of the Mnesia database with observer:start().

> mnesia:transaction(fun() -> mnesia:read(planemo,mercury) end). % read data

QLC (Query List Comprehensions)

> m(qlc). % use q(...) then e(...) in a transaction

-include_lib("stdlib/include/qlc.hrl"). % use this in code, not necessary in the shell

> mnesia:transaction(fun() -> qlc:e(qlc:q([X || X <- mnesia:table(planemo)])) end).                          % returns everything
> mnesia:transaction(fun() -> qlc:e(qlc:q([X || X <- mnesia:table(planemo), X#planemo.gravity < 9.8])) end). % return all planets with gravity < 9.8
> mnesia:transaction(fun() -> qlc:e(qlc:q([{X#planemo.name, X#planemo.gravity} || X <- mnesia:table(planemo),X#planemo.gravity < 9.8] ))end). % restrict the output

OTP (Open Telecom Platform)

> h(gen_server).
> hcb(gen_server).             % callbacks
> hcb(gen_server,handle_call). % details of the handle_call callback
> ht(gen_server,from).         % what is the from() type?

-behaviour(gen_server).
-behavior(gen_server).         % alternative US spelling

> h(supervisor).

supervisor:start_link() calls Module:init() in which the supervisor flags and child specs are specified.
See https://www.erlang.org/doc/man/supervisor.html#start_link-2

sup_flags() = #{strategy => strategy(),           % optional
                intensity => non_neg_integer(),   % optional
                period => pos_integer(),          % optional
                auto_shutdown => auto_shutdown()} % optional

child_spec() = #{id => child_id(),             % mandatory
                 start => mfargs(),            % mandatory
                 restart => restart(),         % optional
                 significant => significant(), % optional
                 shutdown => shutdown(),       % optional
                 type => worker(),             % optional
                 modules => modules()}         % optional

See https://www.erlang.org/doc/man/supervisor.html#Module:init-1

The shell is itself a supervisor, and will terminate processes that report errors!
In the shell to start a supervisor you can unlink the supervisor from the shell.
> {ok, Pid} = drop_sup:start_link().
> unlink(Pid).

The other approach leaves the link in place, but wraps the calls to gen_server/2 in a
catch statement. In this case, using catch just keeps the shell from ever receiving the
exception, so the supervisor remains untouched.
> drop_sup:start_link().
> catch gen_server:call(drop, 60).

You can also tell the shell to stop worrying about such exceptions
by issuing the shell command catch_exception(true).
This turns off the behavior for the entire shell, though.

> h(application).
$ erl -man app

-behavior(application). % just needs a start() and stop() function

The application resource file is to be called Application.app, where
Application is the application name. The file is to be located in
directory ebin for the application.

The file must contain a single Erlang term, which is called an
application specification:

{application, Application,
  [{description,  Description},
   {id,           Id},
   {vsn,          Vsn},
   {modules,      Modules},
   {maxP,         MaxP},
   {maxT,         MaxT},
   {registered,   Names},
   {included_applications, Apps},
   {optional_applications, Apps},
   {applications, Apps},
   {env,          Env},
   {mod,          Start},
   {start_phases, Phases},
   {runtime_dependencies, RTDeps}]}.

> m(application).
> h(application,load).
> application:load(drop).
> application:loaded_applications().
> application:start(drop).
> gen_server:call(drop, 60).
