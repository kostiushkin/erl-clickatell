-module(clickatell).

-behaviour(gen_server).

-export([start_link/3,
         stop/0]).
-export([balance/0,
         check/1,
         cost/1,
         ping/0,
         send/2,
         status/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         ping_loop/2]).
-export([arg_to_sms/1,
         handle/2]).

-define(BASE_URL,  "https://api.clickatell.com").
-define(PING_WAIT, timer:minutes(5)).

%% Starting
start_link(User, Pass, API) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [User, Pass, API], []).

stop() ->
  gen_server:cast(?MODULE, stop).

%% Interface
balance() ->
  gen_server:call(?MODULE, {balance}).

check(To) ->
  gen_server:call(?MODULE, {check, To}).

cost(MessageID) ->
  gen_server:call(?MODULE, {cost, MessageID}).

ping() ->
  gen_server:call(?MODULE, {ping}).

send(To, Message) ->
  gen_server:call(?MODULE, {send, To, Message}).

status(MessageID) ->
  gen_server:call(?MODULE, {status, MessageID}).

%% Server
init([User, Pass, API]) ->
  process_flag(trap_exit, true),
  case call_login(User, Pass, API) of
    {error, Error} ->
      {stop, Error};
    SessionID ->
      spawn_link(?MODULE, ping_loop, [?PING_WAIT, SessionID]),
      {ok, SessionID}
  end.

handle_call({balance}, _From, SessionID) ->
  {reply, call_balance(SessionID), SessionID};
handle_call({check, To}, _From, SessionID) ->
  {reply, call_check(To, SessionID), SessionID};
handle_call({cost, MessageID}, _From, SessionID) ->
  {reply, call_cost(MessageID, SessionID), SessionID};
handle_call({ping}, _From, SessionID) ->
  {reply, call_ping(SessionID), SessionID};
handle_call({send, To, Message}, _From, SessionID) ->
  {reply, call_send(To, Message, SessionID), _From, SessionID};
handle_call({status, MessageID}, _From, SessionID) ->
  {reply, call_status(MessageID, SessionID), _From, SessionID}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info({'EXIT', {ping_failed, Error}}, State) ->
  error_logger:error_info("The ping process failed"),
  {stop, {ping_failed, Error}, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Pinging
ping_loop(Time, SessionID) ->
  error_logger:info_msg("Pinging ~s...~n", [SessionID]),
  case call_ping(SessionID) of
    true           -> timer:sleep(Time), ping_loop(Time, SessionID);
    {error, Error} -> exit({ping_failed, Error})
  end.

%% Sending
call_balance(SessionID) ->
  case call("/http/getbalance", [{session_id, SessionID}]) of
    {ok, PropList} -> list_to_float(proplists:get_value(credit, PropList));
    {error, Error} -> {error, Error}
  end.

call_check(To, SessionID) ->
  case call("/utils/routeCoverage.php", [{msisdn, To}, {session_id, SessionID}]) of
    {ok, _}    -> true;
    {error, _} -> false
  end.

call_cost(MessageID, SessionID) ->
  case call("/http/getmsgcharge", [{apimsgid, MessageID}, {session_id, SessionID}]) of
    {ok, PropList} -> str_to_number(proplists:get_value(charge, PropList));
    {error, Error} -> {error, Error}
  end.

call_login(User, Pass, API) ->
  case call("/http/auth", [{user, User}, {password, Pass}, {api_id, API}]) of
    {ok, PropList} -> proplists:get_value(ok, PropList);
    {error, Error} -> {error, Error}
  end.

call_ping(SessionID) ->
  case call("/http/ping", [{session_id, SessionID}]) of
    {ok, _}        -> true;
    {error, Error} -> {error, Error}
  end.

call_send(To, Message, SessionID) ->
  case call("/http/sendmsg", [{to, To}, {text, Message}, {session_id, SessionID}]) of
    {ok, PropList} -> proplists:get_value(id, PropList);
    {error, Error} -> {error, Error}
  end.

call_status(MessageID, SessionID) ->
  case call("/http/querymsg", [{apimsgid, MessageID}, {session_id, SessionID}]) of
    {ok, PropList} -> str_to_number(proplists:get_value(status, PropList));
    {error, Error} -> {error, Error}
  end.

call(URL, PropList) ->
  {ok,{{_,200,_},_,ResponseText}} = do_request(URL, PropList),
  Response = parse_response(ResponseText),
  case proplists:get_value(err, Response) of
    undefined -> {ok,    Response};
    Error     -> {error, Error}
  end.

do_request(Path, PropList) ->
  URL         = ?BASE_URL ++ Path,
  Headers     = [{"User-Agent", "erl-clickatell"}],
  Payload     = proplist_to_params(PropList),
  ContentType = "application/x-www-form-urlencoded",
  HTTPOptions = [],
  Options     = [],
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
  lists:foldr(fun({Key,Val}, Acc) ->
    yaws_api:url_encode(yaws:to_string(Key)) ++ "=" ++ yaws_api:url_encode(yaws:to_string(Val)) ++
    case Acc of
      "" -> "";
      _  -> "&" ++ Acc
    end
  end, "", PropList).

%% Decoding
parse_response(Str) ->
  {match, Matches} = regexp:matches(Str, "[A-Za-z]+:"),
  parse_response_loop(Str, Matches, []).

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
