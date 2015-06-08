-module(logic_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([start/0]).
-export([try_make_turn/4,who_plays/0, join/1,leave/1,who_won/0,get_cell/2,make_turn/3,get_field/0,reset/0]).

-export([init/1,handle_call/3, handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-define(SERVER, ?MODULE).
-define(LINK, {global, ?SERVER}).

-record(game_state, {
    field=dict:new(),
    name_done=[],
    name_not_done=[],
    winner=atom_about_winner
}).

start_link() ->
    io:format("Starting game_logic. ~n"),
    gen_server:start_link(?LINK, ?MODULE, [], []).

init([]) -> { ok, #game_state{} }.


handle_call( {who_plays} , _, State) -> { reply, who_plays(State), State } ;
handle_call( {join, Name}, _, State) ->
    {Status,NewState}= join_game(Name, State),
    {reply,Status, NewState};
handle_call( {who_won} , _, State) -> {reply, who_won(State), State};
handle_call( {get_cell, X, Y}, _, State) -> {reply, get_cell(X, Y, State), State};
handle_call( {get_field}, _, State) -> {reply, State#game_state.field, State};
handle_call( {make_turn, PlayerName, X, Y}, _, State) ->
    {Status, NewState} = logic_server:try_make_turn(X, Y, PlayerName, State),
    {reply, Status, NewState}.

handle_cast( {leave, Name},State) -> {noreply, leave_game(Name, State)};
handle_cast( {reset}, _ ) -> {noreply, #game_state{}}.

handle_info(_Info, State) -> {noreply, State}.


terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

start() -> start_link().


who_plays()->gen_server:call(?LINK,{who_plays}).

who_plays(State)->State#game_state.name_done++State#game_state.name_not_done.

join(Name)->gen_server:call(?LINK,{join, Name}).

join_game(Name, State)->
    AllPlayers=State#game_state.name_done++State#game_state.name_not_done,
    Check=lists:member(Name,AllPlayers),
    case Check of
        true -> {already_exist, State};
        false ->
            {ok,State#game_state{name_not_done=State#game_state.name_not_done++[Name]}}
    end.

leave(Name)->gen_server:cast(?LINK,{leave, Name}).

leave_game(Name,State)  ->
    All = who_plays(State),
    Size=lists:flatlength(All),
    if Size /=0 ->
        Check1=lists:member(Name,State#game_state.name_done),
        case Check1 of
            true -> S=State#game_state.name_done,State#game_state{name_done=lists:delete(Name,S)};
            false ->
                Check2=lists:member(Name,State#game_state.name_not_done),
                case Check2 of
                    true -> F=State#game_state.name_not_done,State#game_state{name_not_done=lists:delete(Name,F)};
                    false -> State=State
                end
        end;
        true -> State=State
    end.


who_won()->gen_server:call(?LINK,{who_won}).

who_won(State)->State#game_state.winner.

get_cell(X,Y)->gen_server:call(?LINK,{get_cell, X, Y}).

get_cell(X, Y, State) ->
    Check=dict:find({X,Y},State#game_state.field),
    case Check of
        error -> free;
        _ -> occupied
    end.

make_turn(PlayerName, X, Y)->gen_server:call(?LINK,{{make_turn, PlayerName, X, Y}}).

try_make_turn(X, Y, PlayerName, State)->
    CheckCell=get_cell(X,Y,State),
    CheckMove = honest_move(PlayerName,State),
    case CheckMove of
        1  -> case CheckCell of
                  free ->
                      case State#game_state.winner of
                          atom_about_winner  ->
                              Field=dict:append({X,Y},PlayerName,State#game_state.field),
                              CheckWin = check_winner(X,Y,PlayerName,Field),
                              case CheckWin of
                                  true ->
                                      NewState=State#game_state{winner = PlayerName},{game_over_you_win, NewState};
                                  false->
                                      F=State#game_state.name_not_done,
                                      NewState=State#game_state{name_not_done=lists:delete(PlayerName,F),name_done = State#game_state.name_done++[PlayerName],field=Field},
                                      Size=lists:flatlength(NewState#game_state.name_not_done),
                                      case Size of
                                          0 -> New =NewState#game_state{name_not_done=NewState#game_state.name_done, name_done=NewState#game_state.name_not_done},{ok,New};
                                          _ -> {ok,NewState}
                                      end
                              end;
                          _ -> {game_over,State}
                      end;
                  occupied -> {cell_is_occupied,State}
              end;
        0 -> {not_your_turn,State};
        2 -> {not_exist, State}
    end.

honest_move(PlayerName,State) ->
    Check = lists:member(PlayerName,State#game_state.name_done),
    CheckExist = lists:member(PlayerName,State#game_state.name_not_done),
    case Check of
        false ->
            case CheckExist of
                true -> 1;
                false -> 2
            end;
        true-> 0
    end.

check_winner(X,Y,PlayerName,Field) ->

    Horizontal = check_grid(X,Y,PlayerName,Field,x,1,0) - check_grid(X,Y,PlayerName,Field,x,-1,0) - 1,

    Vertical = check_grid(X,Y,PlayerName,Field,y,0,1) - check_grid(X,Y,PlayerName,Field,y,0,-1) - 1,

    Main_diag =  check_grid(X,Y,PlayerName,Field,x,1,-1) - check_grid(X,Y,PlayerName,Field,x,-1,1) - 1,

    Sec_diag = check_grid(X,Y,PlayerName,Field,x,1,1) - check_grid(X,Y,PlayerName,Field,x,-1,-1) - 1,

    case ((Horizontal == 5) or (Vertical == 5) or (Main_diag == 5) or (Sec_diag == 5)) of
        true ->true;
        false -> false
    end.

check_grid(X,Y,PlayerName,Field,Coor,ParamX,ParamY) ->
    Check = dict:find({X,Y},Field),
    case Coor of
        x ->
            case Check of
                {ok, [PlayerName]}  -> check_grid(X+ParamX,Y+ParamY,PlayerName,Field,Coor,ParamX,ParamY);
                _->  X
            end;
        y ->
            case Check of
                {ok, [PlayerName]} -> check_grid(X+ParamX,Y+ParamY,PlayerName,Field,Coor,ParamX,ParamY);
                _ -> Y
            end

    end.

get_field()->gen_server:call(?LINK,{get_field}).

reset()->gen_server:cast(?LINK,{reset}).