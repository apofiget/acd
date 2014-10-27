
-module(acd_sup).

-behaviour(supervisor).

-compile([{parse_transform, lager_transform}]).

-export([start/1]).

-export([init/1]).

-define(CHILD(I, Type, WType, Param), {I, {I, start, Param}, WType, 5000, Type, [I]}).

start(Mod) -> supervisor:start_link({local,
          list_to_atom(atom_to_list(Mod) ++ "_sup")},
          ?MODULE, [Mod]).

init([y_embed]) ->
    io:format("~n***yaws start...~w~n", [self()]),
    YBed = {y_embed, {y_embed, start, []}, permanent, 2000,
      worker, [y_embed]},
    {ok, {{one_for_all, 0, 1}, [YBed]}};

init([Mod]) ->
    io:format("~n***~p start...~w~n", [Mod, self()]),
    RestartStrategy = one_for_one,
    MaxRestarts = 30,
    MaxSecondsBetweenRestarts = 1,
    SupFlags = {RestartStrategy, MaxRestarts,
    MaxSecondsBetweenRestarts},
    {ok, {SupFlags, [?CHILD(Mod,worker,permanent,[])]}}.

