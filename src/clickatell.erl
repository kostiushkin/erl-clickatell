-module(clickatell).

-behaviour(gen_server).

-include("../include/clickatell.hrl").

-export([start_link/3,
         stop/0]).
-export([balance/0,
         check/1,
         cost/1,
         reply/2,
         send/1,
         status/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([arg_to_sms/1,
         handle/2]).

-record(state, {session_id, callback_ets}).

-define(URL,       "https://api.clickatell.com").
-define(TIMEOUT,   timer:seconds(15)).
-define(PING_WAIT, timer:minutes(5)).

%% Starting
start_link(User, Pass, API) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [User, Pass, API], []).

stop() ->
  gen_server:cast(?MODULE, stop).

%% Interface
balance() ->
  gen_server:call(?MODULE, {balance}, ?TIMEOUT).

check(To) ->
  gen_server:call(?MODULE, {check, To}, ?TIMEOUT).

cost(MessageID) ->
  gen_server:call(?MODULE, {cost, MessageID}, ?TIMEOUT).

send(SMS) ->
  gen_server:call(?MODULE, {send, SMS}, ?TIMEOUT).

reply(#sms{to = To, from = From}, Text) ->
  send(#sms{to = From, from = To, text = Text}).

status(MessageID) ->
  gen_server:call(?MODULE, {status, MessageID}, ?TIMEOUT).

%% Server
init([User, Pass, API]) ->
  process_flag(trap_exit, true),
  case login(User, Pass, API) of
    {ok, SessionID} ->
      spawn_link(fun() -> ping_loop(?PING_WAIT, SessionID) end),
      {ok, #state{session_id = SessionID, callback_ets = ets:new(callbacks, [])}};
    {error, Error} ->
      {stop, Error}
  end.

handle_call({balance}, From, State) ->
  {ok, RequestID, Callback} = call_balance(State#state.session_id),
  ets:insert(State#state.callback_ets, {RequestID, From, Callback}),
  {noreply, State};
handle_call({check, To}, From, State) ->
  {ok, RequestID, Callback} = call_check(To, State#state.session_id),
  ets:insert(State#state.callback_ets, {RequestID, From, Callback}),
  {noreply, State};
handle_call({cost, MessageID}, From, State) ->
  {ok, RequestID, Callback} = call_cost(MessageID, State#state.session_id),
  ets:insert(State#state.callback_ets, {RequestID, From, Callback}),
  {noreply, State};
handle_call({send, SMS}, From, State) ->
  {ok, RequestID, Callback} = call_send(SMS, State#state.session_id),
  ets:insert(State#state.callback_ets, {RequestID, From, Callback}),
  {noreply, State};
handle_call({status, MessageID}, From, State) ->
  {ok, RequestID, Callback} = call_status(MessageID, State#state.session_id),
  ets:insert(State#state.callback_ets, {RequestID, From, Callback}),
  {noreply, State}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info({http, {RequestID, {error, Error}}}, State) ->
  {stop, {http_error, Error, RequestID}, State};
handle_info({http, {RequestID, HTTPResponse}}, State) ->
  [{_, From, Callback}] = ets:lookup(State#state.callback_ets, RequestID),
  gen_server:reply(From, apply(Callback, [HTTPResponse])),
  ets:delete(State#state.callback_ets, RequestID),
  {noreply, State};
handle_info({'EXIT', _Pid, {ping_error, Error}}, State) ->
  {stop, {ping_error, Error}, State}.

terminate({ping_error, Error}, State) ->
  error_logger:error_msg("Failed to ping ~s because ~p", [State#state.session_id, Error]),
  ok;
terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Sessions
login(User, Pass, API) ->
  {ok, HTTPResponse} = call("/http/auth", [{user, User}, {password, Pass}, {api_id, API}]),
  case parse_response(HTTPResponse) of
    {ok, PropList} -> {ok, proplists:get_value(ok, PropList)};
    {error, Error} -> {stop, Error}
  end.

ping_loop(Time, SessionID) ->
  error_logger:info_msg("Pinging ~s...~n", [SessionID]),
  {ok, HTTPResponse} = call("/http/ping", [{session_id, SessionID}]),
  case parse_response(HTTPResponse) of
    {ok, _}        -> timer:sleep(Time), ping_loop(Time, SessionID);
    {error, Error} -> exit({ping_error, Error})
  end.

%% Commands
call_balance(SessionID) ->
  callback("/http/getbalance", [{session_id, SessionID}], fun handle_balance/1).
handle_balance(HTTPResponse) ->
  case parse_response(HTTPResponse) of
    {ok, PropList} -> list_to_float(proplists:get_value(credit, PropList));
    {error, Error} -> {error, Error}
  end.

call_check(To, SessionID) ->
  callback("/utils/routeCoverage.php", [{msisdn, To}, {session_id, SessionID}], fun handle_check/1).
handle_check(HTTPResponse) ->
  case parse_response(HTTPResponse) of
    {ok, _}    -> true;
    {error, _} -> false
  end.

call_cost(MessageID, SessionID) ->
  callback("/http/getmsgcharge", [{apimsgid, MessageID}, {session_id, SessionID}], fun handle_cost/1).
handle_cost(HTTPResponse) ->
  case parse_response(HTTPResponse) of
    {ok, PropList} -> str_to_number(proplists:get_value(charge, PropList));
    {error, Error} -> {error, Error}
  end.

call_send(#sms{to = To, from = From, text = Text, options = Opts}, SessionID) ->
  callback("/http/sendmsg", [{to, To}, {from, From}, {text, Text}, {session_id, SessionID}] ++ Opts, fun handle_send/1).
handle_send(HTTPResponse) ->
  case parse_response(HTTPResponse) of
    {ok, PropList} -> proplists:get_value(id, PropList);
    {error, Error} -> {error, Error}
  end.

call_status(MessageID, SessionID) ->
  callback("/http/querymsg", [{apimsgid, MessageID}, {session_id, SessionID}], fun handle_status/1).
handle_status(HTTPResponse) ->
  case parse_response(HTTPResponse) of
    {ok, PropList} -> str_to_number(proplists:get_value(status, PropList));
    {error, Error} -> {error, Error}
  end.

%% Sending
call(Path, PropList) ->
  call(Path, PropList, true).

call(Path, PropList, Sync) ->
  URL         = ?URL ++ Path,
  Headers     = [{"User-Agent", "erl-clickatell"}],
  Payload     = proplist_to_params(PropList),
  ContentType = "application/x-www-form-urlencoded",
  HTTPOptions = [],
  Options     = [{sync, Sync}, {body_format, binary}],
  http:request(post, {URL, Headers, ContentType, Payload}, HTTPOptions, Options).

callback(Path, PropList, Callback) when is_function(Callback) ->
  {ok, RequestID} = call(Path, PropList, false),
  {ok, RequestID, Callback}.

%% Receiving
arg_to_sms(Arg) ->
  PropList = arg_to_proplist(Arg),
  To       = list_to_integer(proplists:get_value("to",   PropList)),
  From     = list_to_integer(proplists:get_value("from", PropList)),
  Text     = proplists:get_value("text", PropList),
  {ok, #sms{to = To, from = From, text = Text, options = PropList}}.

handle(Arg, {M, F}) ->
  case arg_to_sms(Arg) of
    {ok, SMS}      -> M:F(SMS);
    {error, Error} -> M:F({error, Error})
  end,
  ok.

%% Utils
str_to_number(Str) ->
  try              list_to_integer(Str)
  catch error:_ -> list_to_float(Str)
  end.

proplist_to_params(PropList) ->
  join("&", lists:map(fun({Key, Val}) ->
    yaws_api:url_encode(yaws:to_string(Key)) ++ "=" ++ yaws_api:url_encode(yaws:to_string(Val))
  end, PropList)).

arg_to_proplist(Arg) ->
  lists:map(fun({Key,Val}) -> {list_to_atom(Key), Val} end, yaws_api:parse_query(Arg)).

parse_response(HTTPResponse) ->
  case HTTPResponse of
    {{_, 200, _}, _, ResponseBinary} ->
      Str              = binary_to_list(ResponseBinary),
      {match, Matches} = regexp:matches(Str, "[A-Za-z]+:"),
      PropList         = parse_response_loop(Str, Matches, []),
      case proplists:get_value(err, PropList) of
        undefined -> {ok,    PropList};
        Error     -> {error, Error}
      end;
    {{_, Error, _}, _, _} ->
      {error, Error}
  end.

parse_response_loop(Str, Matches, PropList) ->
  case Matches of
    [{NextPt, PtLen}] ->
      lists:reverse([{parse_left (string:substr(Str, NextPt,  PtLen)),
                      parse_right(string:substr(Str, NextPt + PtLen))}
                    | PropList]);
    [{NextPt1, PtLen1}, {NextPt2, PtLen2} | RemMatches] ->
      parse_response_loop(Str,
                          [{NextPt2, PtLen2} | RemMatches],
                          [{parse_left (string:substr(Str, NextPt1,  PtLen1)),
                            parse_right(string:substr(Str, NextPt1 + PtLen1,
                                                           NextPt2 - (NextPt1 + PtLen1)))}
                          | PropList])
 end.

parse_left(Str) ->
  list_to_atom(string:strip(http_util:to_lower(lists:delete($:, Str)))).

parse_right(Str) ->
  string:strip(Str).

join(Sep, List) ->
  lists:foldl(fun(A, "") -> A; (A, Acc) -> Acc ++ Sep ++ A end, "", List).
