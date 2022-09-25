%%%-------------------------------------------------------------------
%% @doc scp mid level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sub_sup).

-behaviour(supervisor).

-export([start_link/1, start_child/1]).

-export([init/1, gen_name/3, bytes_generate/1]).

-define(SERVER, ?MODULE).
-define(CHILD(Ns, Np, Payload), {gen_name("server_", Ns, Np), {process_pair, spawn_server, [Payload]}, transient, worker, [process_pair]}).

start_link(Args) ->
    supervisor:start_link(?MODULE, [Args]).

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

    {NumSup, NumPairs} = Args,

    SupFlags = #{strategy => one_for_one,
                 intensity => 100000,
                 period => 1},

    ChildSpecs = [ ?CHILD(NumSup, N, bytes_generate(500)) || N <- lists:seq(NumPairs)],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

bytes_generate(Size) ->
    bytes_generate(Size, []).

bytes_generate(0, Bytes) ->
    list_to_binary(Bytes);

bytes_generate(Size, Bytes) ->
    bytes_generate(Size - 1, [1 | Bytes]).

gen_name(Type, Ns, Np) ->
    list_to_atom(string:concat(Type, string:concat(integer_to_list(Ns), string:concat("_", integer_to_list(Np))))).