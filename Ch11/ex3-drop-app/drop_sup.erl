-module(drop_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

%%%%%%%
% API %
%%%%%%%

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%%%%%%%%%%
% Callback %
%%%%%%%%%%%%

init([]) ->
  SupFlags =
    #{strategy => one_for_one,
      intensity => 100,
      period => 1},
  Drop =
    #{id => drop,
      start => {drop, start_link, []},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [drop]},
  {ok, {SupFlags, [Drop]}}.
