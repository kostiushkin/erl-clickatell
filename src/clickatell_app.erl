-module(clickatell_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
  {ok, User} = application:get_env(clickatell, user),
  {ok, Pass} = application:get_env(clickatell, pass),
  {ok, API}  = application:get_env(clickatell, api),
  clickatell_sup:start_link(User, Pass, API).

stop(_State) ->
  ok.
