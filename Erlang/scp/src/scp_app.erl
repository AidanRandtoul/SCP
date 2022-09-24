%%%-------------------------------------------------------------------
%% @doc scp public API
%% @end
%%%-------------------------------------------------------------------

-module(scp_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    scp_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
