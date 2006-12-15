-module(clickatell).

-behaviour(gen_server).

-export([start_link/3,
         stop/0]).
-export([balance/0,
         check/1,
         cost/1,
         send/2,
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

-define(URL,      "https://api.clickatell.com").
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

send(To, Message) ->
  gen_server:call(?MODULE, {send, To, Message}, ?TIMEOUT).

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
  {ok, RequestID} = call_balance(State#state.session_id),
  ets:insert(State#state.callback_ets, {RequestID, {balance, From}}),
  {noreply, State};
handle_call({check, To}, From, State) ->
  {ok, RequestID} = call_check(To, State#state.session_id),
  ets:insert(State#state.callback_ets, {RequestID, {check, From}}),
  {noreply, State};
handle_call({cost, MessageID}, From, State) ->
  {ok, RequestID} = call_cost(MessageID, State#state.session_id),
  ets:insert(State#state.callback_ets, {RequestID, {cost, From}}),
  {noreply, State};
handle_call({send, To, Message}, From, State) ->
  {ok, RequestID} = call_send(To, Message, State#state.session_id),
  ets:insert(State#state.callback_ets, {RequestID, {send, From}}),
  {noreply, State};
handle_call({status, MessageID}, From, State) ->
  {ok, RequestID} = call_status(MessageID, State#state.session_id),
  ets:insert(State#state.callback_ets, {RequestID, {status, From}}),
  {noreply, State}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info({http, {RequestID, HTTPResponse}}, State) ->
  Response               = parse_response({ok, HTTPResponse}),
  [{_, {Command, From}}] = ets:lookup(State#state.callback_ets, RequestID),
  case Command of
    balance -> gen_server:reply(From, handle_balance(Response));
    check   -> gen_server:reply(From, handle_check(Response));
    cost    -> gen_server:reply(From, handle_cost(Response));
    send    -> gen_server:reply(From, handle_send(Response));
    status  -> gen_server:reply(From, handle_status(Response))
  end,
  ets:delete(State#state.callback_ets, RequestID),
  {noreply, State};
handle_info({'EXIT', {ping_failed, Error}}, State) ->
  error_logger:error_info("The ping process failed"),
  {stop, {ping_failed, Error}, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Sessions
login(User, Pass, API) ->
  HTTPResponse = call("/http/auth", [{user, User}, {password, Pass}, {api_id, API}], true),
  case parse_response(HTTPResponse) of
    {ok, PropList} -> {ok, proplists:get_value(ok, PropList)};
    {error, Error} -> {stop, Error}
  end.

ping(SessionID) ->
  HTTPResponse = call("/http/ping", [{session_id, SessionID}], true),
  case parse_response(HTTPResponse) of
    {ok, _}        -> true;
    {error, Error} -> {error, Error}
  end.

ping_loop(Time, SessionID) ->
  error_logger:info_msg("Pinging ~s...~n", [SessionID]),
  case ping(SessionID) of
    true           -> timer:sleep(Time), ping_loop(Time, SessionID);
    {error, Error} -> exit({ping_failed, Error})
  end.

%% Sending
call_balance(SessionID) ->
  call("/http/getbalance", [{session_id, SessionID}]).
handle_balance({ok, PropList}) ->
  list_to_float(proplists:get_value(credit, PropList));
handle_balance({error, Error}) ->
  {error, Error}.

call_check(To, SessionID) ->
  call("/utils/routeCoverage.php", [{msisdn, To}, {session_id, SessionID}]).
handle_check({ok, _}) ->
  true;
handle_check({error, _}) ->
  false.

call_cost(MessageID, SessionID) ->
  call("/http/getmsgcharge", [{apimsgid, MessageID}, {session_id, SessionID}]).
handle_cost({ok, PropList}) ->
  str_to_number(proplists:get_value(charge, PropList));
handle_cost({error, Error}) ->
  {error, Error}.

call_send(To, Message, SessionID) ->
  call("/http/sendmsg", [{to, To}, {text, Message}, {session_id, SessionID}]).
handle_send({ok, PropList}) ->
  proplists:get_value(id, PropList);
handle_send({error, Error}) ->
  {error, Error}.

call_status(MessageID, SessionID) ->
  call("/http/querymsg", [{apimsgid, MessageID}, {session_id, SessionID}]).
handle_status({ok, PropList}) ->
  str_to_number(proplists:get_value(status, PropList));
handle_status({error, Error}) ->
  {error, Error}.

call(Path, PropList) ->
  call(Path, PropList, false).

call(Path, PropList, Sync) ->
  URL         = ?URL ++ Path,
  Headers     = [{"User-Agent", "erl-clickatell"}],
  Payload     = proplist_to_params(PropList),
  ContentType = "application/x-www-form-urlencoded",
  HTTPOptions = [],
  Options     = [{sync, Sync}, {body_format, binary}],
  http:request(post, {URL, Headers, ContentType, Payload}, HTTPOptions, Options).

%% Recieving
arg_to_sms(Arg) ->
  PropList = yaws_api:parse_query(Arg),
  From = list_to_integer(proplists:get_value("from", PropList)),
  Text = proplists:get_value("text", PropList),
  {ok, {From, Text, PropList}}.

handle(Arg, {M, F}) ->
  case arg_to_sms(Arg) of
    {ok, SMS}      -> M:F({message, SMS});
    {error, Error} -> M:F({error, Error})
  end,
  ok.

%% Coercion
str_to_number(Str) ->
  try              list_to_integer(Str)
  catch error:_ -> list_to_float(Str)
  end.

%% Encoding
proplist_to_params(PropList) ->
  join("&", lists:map(fun({Key, Val}) ->
    yaws_api:url_encode(yaws:to_string(Key)) ++ "=" ++ yaws_api:url_encode(yaws:to_string(Val))
  end, PropList)).

%% Decoding
parse_response(HTTPResponse) ->
  case HTTPResponse of
    {ok, {{_, 200, _}, _, ResponseBinary}} ->
      Str              = binary_to_list(ResponseBinary),
      {match, Matches} = regexp:matches(Str, "[A-Za-z]+:"),
      PropList         = parse_response_loop(Str, Matches, []),
      case proplists:get_value(err, PropList) of
        undefined -> {ok,    PropList};
        Error     -> {error, Error}
      end;
    {ok, {{_, Error, _}, _, _}} ->
      {error, Error};
    {error, Error} ->
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

%% Utils
join(Sep, List) ->
    lists:foldl(fun(A, "") -> A; (A, Acc) -> Acc ++ Sep ++ A end, "", List).
