-module(clickatell_sup).

-behaviour(supervisor).

-export([start_link/3, init/1]).

start_link(User, Pass, API) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [User, Pass, API]).

init([User, Pass, API]) ->
  RestartStrategy    = one_for_one,
  MaxRestarts        = 1000,
  MaxTimeBetRestarts = 3600,

  SupFlags = {RestartStrategy, MaxRestarts, MaxTimeBetRestarts},

  ChildSpecs =
  [
   {clickatell,
    {clickatell, start_link, [User, Pass, API]},
    permanent,
    1000,
    worker,
    [clickatell]}
   ],
  {ok,{SupFlags, ChildSpecs}}.
