-module(drop_sup).

-behavior(supervisor).

% API
-export([start_link/0]).
% Callback
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
      intensity => 1,
      period => 5},
  Drop =
    #{id => drop,
      start => {drop, start_link, []},
      restart => permanent,
      shutdown => 5000,
      type => worker,
      modules => [drop]},
  {ok, {SupFlags, [Drop]}}.
