%%%-------------------------------------------------------------------
%% @doc scp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(head_supervisor).

-behaviour(supervisor).

-export([start_child/1, start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Args) ->
    supervisor:start_child(?MODULE, Args).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional

init(_) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 100,
                 period => 1},
    {ok, {SupFlags, child_specs()}}.

%% internal functions

child_specs() ->
    [#{id => undefined,
       start => {ftp, start_link, []},
       restart => temporary,
       shutdown => 4000,
       type => worker,
       modules => [ftp]}].