%%%-------------------------------------------------------------------
%% @doc scp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(head_sup).

-behaviour(supervisor).

-export([start_child/1, start_link/1]).

-export([init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(N, NumPairs), {gen_name("sub_sup_", N), {sub_sup, start_link, [N, NumPairs]}, transient, supervisor, [sub_sup]}).

start_link(Args) ->
    supervisor:start_link({local, head_sup}, ?MODULE, [Args]).

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

init(Args) ->

    {NumPairs, NumSups} = Args,

    SupFlags = #{strategy => one_for_one,
                 intensity => 100,
                 period => 1},

    Children = [ ?CHILD(N, NumPairs) || N <- list:seq(1, NumSups) ],

    {ok, {SupFlags, Children}}.

%% internal functions

gen_name(Type, N) ->
    list_to_atom(string:concat(Type, integer_to_list(N))).