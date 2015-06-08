-module(game_server).
-export([who_plays/3,join/3,leave/3,who_won/3,make_turn/3,reset/3,get_field/3]).
-import(mod_esi, [deliver/2]).

-define(LOGIC, {global, logic_server}).

ct_string(json) -> "Content-type: application/json\r\n\r\n";
ct_string(text) -> "Content-type: text/plain\r\n\r\n".

who_plays(SessionId, _, _)->
  Name=gen_server:call(?LOGIC,{who_plays}),
  NameJSON="[" ++ string:join(lists:map(fun(X)->io_lib:format("\"~s\"", [X])
  end, Name), ", ") ++ "]",
  Finish = io_lib:format("{\"players\": ~s}",[NameJSON]),
  mod_esi:deliver(SessionId, ct_string(json) ++ Finish).


join(SessionId, _, In) ->
  Name = http_uri:decode(In),
  Status = gen_server:call(?LOGIC, {join, Name}),
  mod_esi:deliver(SessionId, ct_string(text) ++ atom_to_list(Status)).


leave(SessionId, _, In) ->
  Name = http_uri:decode(In),
  gen_server:cast(?LOGIC, {leave, Name}),
  mod_esi:deliver(SessionId, ct_string(text) ++ "ok").

who_won(SessionId, _, _) ->
  Name = gen_server:call(?LOGIC, {who_won}),
  case Name of
    atom_about_winner -> deliver(SessionId, ct_string(json) ++ "null");
    _ -> deliver(SessionId, ct_string(json) ++ io_lib:format("{\"~s\"}", [Name]))
  end.

make_turn(SessionId, _, In) ->
  Request = http_uri:decode(In),
  WordsCount = string:words(Request, 47),
  case WordsCount of
    3 -> Name = string:sub_word(Request, 1, 47),
      {X, _} = string:to_integer(string:sub_word(Request, 2, 47)),
      {Y, _} = string:to_integer(string:sub_word(Request, 3, 47)),
      Status = gen_server:call(?LOGIC, {make_turn, Name, X, Y}),
      deliver(SessionId, ct_string(text) ++ atom_to_list(Status));
    _ -> deliver(SessionId, ct_string(text) ++ "bad_request")
  end.

get_field(SessionId, _, _) ->
  FieldItems = dict:to_list(gen_server:call(?LOGIC, {get_field})),
  ScreenedItems = lists:map(fun({{X,Y}, Value}) ->
    io_lib:format("{\"x\": ~p, \"y\": ~p, \"player\": \"~s\"}", [X, Y, Value])
  end, FieldItems),
  FieldJSON = "[" ++ string:join(ScreenedItems, ", ") ++ "]",
  mod_esi:deliver(SessionId, ct_string(json) ++ FieldJSON).


reset(SessionId,_,_) ->
  gen_server:cast(?LOGIC, {reset}),
  deliver(SessionId, ct_string(text) ++ "reset").

