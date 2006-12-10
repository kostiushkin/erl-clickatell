-module(clickatell_utils).

-export([errormsg/1,
         errorcode/1]).
-export([to_list/1,
         str_to_number/1]).
-export([proplist_to_params/1,
         parse_response/1]).

%% Errors
errormsg(String) ->
  string:substr(String, 5).

errorcode(String) ->
  list_to_integer(string:substr(String, 1, 3)).

%% Coercion
to_list(X) ->
  if list(X)    -> X;
     atom(X)    -> atom_to_list(X);
     integer(X) -> integer_to_list(X)
  end.

str_to_number(Str) ->
  try              list_to_integer(Str)
  catch error:_ -> list_to_float(Str)
  end.

%% Encoding
proplist_to_params(PropList) ->
  lists:foldr(fun({Key,Val}, Acc) ->
    yaws_api:url_encode(to_list(Key)) ++ "=" ++ yaws_api:url_encode(to_list(Val)) ++
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
