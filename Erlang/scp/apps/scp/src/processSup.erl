-module(processSup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link({Name, Spec}) ->
    supervisor:start_link({local, Name}, ?MODULE, Spec).

init(Spec) ->
    {ok, Spec}.


% init({{RestartStrategy, MaxRestart, MaxTime}, Children}) ->
    % {ok, {{RestartStrategy, MaxRestart, MaxTime}, Children}}.