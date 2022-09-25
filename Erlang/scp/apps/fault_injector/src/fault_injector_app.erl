%%%-------------------------------------------------------------------
%% @doc fault injector public API
%% @end
%%%-------------------------------------------------------------------

-module(fault_injector_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    fault_injector:start(burst).

stop(_State) ->
    ok.

%% internal functions
