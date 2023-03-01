-module(recordDrop).

-export([fall_velocity/1]).

-include("records.hrl").

fall_velocity(#tower{planemo = Planemo, height = Distance}) ->
  fall_velocity(Planemo, Distance).

fall_velocity(earth, Distance) when Distance >= 0 ->
  math:sqrt(2 * 9.8 * Distance);
fall_velocity(moon, Distance) when Distance >= 0 ->
  math:sqrt(2 * 1.6 * Distance);
fall_velocity(mars, Distance) when Distance >= 0 ->
  math:sqrt(2 * 3.71 * Distance).
