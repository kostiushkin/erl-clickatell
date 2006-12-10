-module(clickatell_sup).

-behaviour(supervisor).

-export([start_link/3, init/1]).

start_link(User, Pass, API) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [User, Pass, API]).

init([User, Pass, API]) ->
  Process = {clickatell, {clickatell, start_link, [User, Pass, API]},
             permanent, 10, worker, [clickatell]},
  {ok, {{one_for_one, 3, 10}, [Process]}}.
