%%%-------------------------------------------------------------------
%%% @author  <krishnakumar@KRISHNAKUMAR-HP>
%%% @copyright (C) 2017, 
%%% @doc
%%%
%%% @end
%%% Created :  5 Jan 2017 by  <krishnakumar@KRISHNAKUMAR-HP>
%%%-------------------------------------------------------------------
-module(newsweep).

-behaviour(gen_server).

%% API
-export([start/0,o/2,f/2,uf/2,load/1,reset/0,state/0,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,{allRows = [], %%List of each row data
	       flag_count, %%Flag_count will be equal to mines available
	       mines = [], %% List of mines
	       xlimit, %%Xbound for input
	       ylimit, %%Ybound for input
	       opened = [], %%List of opened cell coordinates{Y,X}
	       flagged = [], %%List of flagged cell coordinates{Y,X}
	       ready = false}). %%True when loaded

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start() ->
    %%Started in Standalone mode
    gen_server:start({local,?MODULE},?MODULE,[],[{timeout,infinity}]).

%%--------------------------------------------------------------------
%% @doc
%% Opens a cell
%%
%% @spec o(Y,X) -> {ok,CurrentState} | {invalid_bound}
%% @end
%%--------------------------------------------------------------------
o(Y,X) ->
    gen_server:call(?MODULE,{open,Y,X},1000).

%%--------------------------------------------------------------------
%% @doc
%% Flags a cell
%%
%% @spec o(Y,X) -> {ok,CurrentState} | {invalid_bound}
%% @end
%%--------------------------------------------------------------------
f(Y,X) ->
    gen_server:call(?MODULE,{flag,Y,X},1000).

%%--------------------------------------------------------------------
%% @doc
%% Tries to UnFlag a cell
%%
%% @spec o(Y,X) -> {ok,CurrentState} | {invalid_bound}
%% @end
%%--------------------------------------------------------------------
uf(Y,X) ->
    gen_server:call(?MODULE,{unflag,Y,X},1000).

%%--------------------------------------------------------------------
%% @doc
%% Load the game string
%% Eg:"xxm,xmx,xxx"
%%
%% @spec load(String) -> ok | {invalid_input,String} | {mines,MinesList}
%%                       {invalid_data,CurrentState}
%% @end
%%--------------------------------------------------------------------
load(String) ->
    gen_server:call(?MODULE,{load,String},1000).

%%--------------------------------------------------------------------
%% @doc
%% Resets the game
%%
%% @spec reset(String) -> {reset,CurrentState}
%% @end
%%--------------------------------------------------------------------
reset() ->
    gen_server:call(?MODULE,reset_game,1000).

%%--------------------------------------------------------------------
%% @doc
%% prints the current state of the game
%%
%% @spec state() -> {ok,CurrentState}
%% @end
%%--------------------------------------------------------------------
state() ->
    gen_server:call(?MODULE,state,1000).

%%--------------------------------------------------------------------
%% @doc
%% Stops the game
%%
%% @spec stop() -> ok | {error, Error}
%% @end
%%--------------------------------------------------------------------
stop() ->
    gen_server:stop(?MODULE).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok,#state{opened = [],flagged = []}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

%% Prints the current state of the game
handle_call(state,_From,State) when State#state.ready =:= true->
    print_state(State,[]),
    CurrentState = get_current_state(State,[]),
    {reply,{ok,CurrentState},State};

%% Resets a Game and prints the current State
handle_call(reset_game,_From,State) when State#state.ready =:= true->
    NewState = reset_game(State),
    io:format("~n Game is RESET ~n"),
    Reset = get_current_state(NewState,[]),
    {reply,{reset,Reset},NewState};

%%Loads game data string
handle_call({load,String},_From,State) ->
    case validate_game_string(String) of
	{true,AllRows,Xlimit,Ylimit} ->
	    case find_mines(AllRows) of
		invalid ->
		    io:format("Invalid characters in the input data,retry~n"),
		    {reply,{invalid_data,get_current_state(State,[])},
		     reset_game(State#state{allRows=[]})};
		Mines ->
		    %%	    io:format("Mines located at ~p~n",[Mines]),
		    NewState = reset_game(State),
		    {reply,{mines,Mines},NewState#state{allRows = AllRows,
						mines = Mines,
						flag_count = length(Mines),
						xlimit = Xlimit,
						ylimit = Ylimit,
						ready = true
					       }}
	    end;
	false ->
	    io:format("Invalid input game data~n"),
	    {reply,{invalid_input,String},State}
    end;

%% Opens a cell on the minefield
handle_call({open,Y,X},_From,State) when State#state.ready=:=true->
    Ylimit = State#state.ylimit,
    Xlimit = State#state.xlimit,
    Mines = State#state.mines,
    Opened = State#state.opened,
    Flagged = State#state.flagged,
    FlagCount = State#state.flag_count,
    case {Y,X} of
	Input when element(1,Input) < Ylimit,element(2,Input) < Xlimit ->
	    NewState = case lists:member(Input,Mines) of
			   true ->
			       io:format("Oops,you stepped over a mine!Gameover~n"),
			       print_state(State,{Y,X}),
			       reset_game(State);
			   false ->
			       case lists:member({Y,X},Flagged) of
				   true ->
				       NowState = State#state{flagged=
								  Flagged--[{Y,X}],
							      opened=
								  [Input|Opened],
							     flag_count=
								  FlagCount+1},
				       case check_game_done(NowState) of
					   won ->
					       print_state(NowState,[]),
					       reset_game(NowState);
					   continue ->
					       print_state(NowState,[]),
					       NowState
				       end;
				   false ->
				       case lists:member({Y,X},Opened) of
					   true -> 
					       State;
					   false ->
					       NowState = State#state{opened = [Input|Opened]},
					       case check_game_done(NowState) of
						   won ->
						       print_state(NowState,[]),
						       reset_game(NowState);
						   continue ->
						       print_state(NowState,[]),
						       NowState
					       end
				       end
			       end
		       end,
	    {reply,{ok,get_current_state(NewState,[])},NewState};
	_ ->
	    io:format("Invalid bounds~n"),
	    {reply,{invalid_bound},State}
    end;

%%Flags a Cell in the minefield
handle_call({flag,Y,X},_From,State) when State#state.ready =:= true->
    Ylimit = State#state.ylimit,
    Xlimit = State#state.xlimit,
    Flagged = State#state.flagged,
    case {Y,X} of
	{Y,X} when Y<Ylimit,X<Xlimit ->
	    case State#state.flag_count of
		0 ->
		    io:format("All flags are exhausted,unflag few~n"),
		    print_state(State,[]),
		    {reply,{flags_exhaust,get_current_state(State,[])},State};
		FlagCount ->
		    case lists:member({Y,X},Flagged) of
			true ->
			    print_state(State,[]),
			    {reply,{ok,get_current_state(State,[])},State};
			false ->
			    NowState = State#state{flagged=[{Y,X}|Flagged]},
			    NewState = NowState#state{flag_count = FlagCount - 1},
			    print_state(NewState,[]),
			    {reply,{ok,get_current_state(NewState,[])},NewState}
			end
	    end;
	_ ->
	    io:format("Invalid bounds~n"),
	    print_state(State,[]),		
	    {reply,{invalid_bound},State}
    end;

%%Unflags a cell in the minefield
handle_call({unflag,Y,X},_From,State) when State#state.ready =:= true->
    Ylimit = State#state.ylimit,
    Xlimit = State#state.xlimit,
    Flagged = State#state.flagged,
    FlagCount = State#state.flag_count,
    case {Y,X} of
	{Y,X} when Y<Ylimit,X<Xlimit ->
	    NewState = case lists:member({Y,X},Flagged)of
			   false ->
			       io:format("Unflag not possible ~n"),
			       print_state(State,[]),
			       State;
			   true ->
			       case lists:member({Y,X},Flagged) of
				   false ->
				       print_state(State,[]),
				       State;
				   true ->
				       NowState = State#state{flagged=Flagged--[{Y,X}]},
				       UpState = NowState#state{flag_count = FlagCount+1},
				       print_state(UpState,[]),
				       UpState
			       end
		       end,
		       {reply,{ok,get_current_state(NewState,[])},NewState};
	_ ->
	    io:format("Invalid bounds~n"),
	    print_state(State,[]),		
	    {reply,{invalid_bound},State}
    end;

%%When all the above catches are not matched
handle_call(_,_From,State) ->
    io:format("No game data loaded,load game first~n"),
    {reply,{no_data_loaded},State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    io:format(" GAME STOPPED ~n"),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.





%%%===================================================================
%%%===================================================================
%%% Internal functions
%%%===================================================================
%%%===================================================================

%%--------------------------------------------------------------------
%% Validates whether the given string is m X n array
%%--------------------------------------------------------------------
validate_game_string("") ->
    %%io:format("Invalid input game string~n");
    false;
validate_game_string(String) when is_list(String)->
    AllRows = [H|_Xrows] = string:tokens(String,","),
    LenXrows = length(AllRows),
    LenXrow = length(H),
    ValidRowLen = length([LenXrow||EachXrow<-AllRows,LenXrow =:= length(EachXrow)]),
    case ValidRowLen of
	LenXrows ->
	    {true,AllRows,LenXrow,LenXrows};
	_ ->
	    %%io:format("input game string is not valid~n"),
	    false
    end.

%%--------------------------------------------------------------------
%% Returns a list of mines, otherwise invalid
%%--------------------------------------------------------------------
find_mines(AllRows) ->
    find_mine_yiter(AllRows,0,[]).

find_mine_yiter([],_Yiter,Acc) ->
    Acc;
find_mine_yiter([Row|Rest],Yiter,Acc)->
    case find_mine_xiter(Row,0,Yiter,[]) of
	invalid ->
	    invalid;
	XiterAcc ->
	    find_mine_yiter(Rest,Yiter+1,XiterAcc++Acc)
    end.

find_mine_xiter([],_Xiter,_Yiter,Acc) ->
    Acc;
find_mine_xiter([C|Rest],Xiter,Yiter,Acc) when C=:=109->
    find_mine_xiter(Rest,Xiter+1,Yiter,[{Yiter,Xiter}|Acc]);
find_mine_xiter([C|Rest],Xiter,Yiter,Acc) when C=:=120 orelse C=:=102->
    find_mine_xiter(Rest,Xiter+1,Yiter,Acc);
find_mine_xiter(_,_Xiter,_Yiter,_Acc) ->
    invalid.

%%--------------------------------------------------------------------
%% Returns the status of the game,
%% won --> Game won by the player
%% continue --> Game not won
%%--------------------------------------------------------------------
check_game_done(State) ->
    FlaggedLen = length(State#state.flagged),
    OpenedLen = length(State#state.opened),
    MineLen = length(State#state.mines),
    Totalpossib = State#state.xlimit * State#state.ylimit,
    Diff = Totalpossib - MineLen,
    case Diff of
   	OpenedLen ->
	    case MineLen of
		FlaggedLen ->
		    io:format("Gameover, you won~n"),
		    reset_game(State),
		    won;
		_ ->
		    continue
	    end;
	_ ->
	    continue
    end.

%%--------------------------------------------------------------------
%% Resets the state of the Game, the actual loaded data is not lost
%%--------------------------------------------------------------------
reset_game(State) ->
    State#state{flagged = [],
		opened = []}.

%%--------------------------------------------------------------------
%% Returns the current state of the game, returns a list of row strings
%%--------------------------------------------------------------------
get_current_state(State,Mine) ->
    Flagged = State#state.flagged,
    Opened = State#state.opened,
    Xlimit = State#state.xlimit,
    Ylimit = State#state.ylimit,
    form_print(Opened,Flagged,Xlimit,Ylimit,Mine).   

%%--------------------------------------------------------------------
%% Works the same way as get_current_state, also has side effect to
%% to print the state on the terminal
%%--------------------------------------------------------------------
print_state(State,Mine) ->
    FlagCount = State#state.flag_count,
    Flagged = State#state.flagged,
    Opened = State#state.opened,
    Xlimit = State#state.xlimit,
    Ylimit = State#state.ylimit,
    MsgToPrint = form_print(Opened,Flagged,Xlimit,Ylimit,Mine),
    io:format("~n-----~n"),
    io:format("flags Remain ~p~n",[FlagCount]),
    [io:format("~s~n",[EachRow])||EachRow<-MsgToPrint],
    io:format("-----~n").

form_print(Opened,Flagged,Xlimit,Ylimit,Mine) ->
    form_yiter(Ylimit,Xlimit,Opened,Flagged,[],Mine).

form_yiter(0,_Xiter,_Opened,_Flagged,Acc,_Mine) ->
    Acc;
form_yiter(Yiter,Xiter,Opened,Flagged,Acc,Mine) ->
    XiterReturn = form_xiter(Yiter,Xiter,Opened,Flagged,[],Mine),
    form_yiter(Yiter-1,Xiter,Opened,Flagged,[XiterReturn|Acc],Mine).

form_xiter(_Yiter,0,_Opened,_Flagged,Acc,_Mine) ->
    Acc;
form_xiter(Yiter,Xiter,Opened,Flagged,Acc,Mine) ->
    case {Yiter-1,Xiter-1} of
	Mine ->
	    form_xiter(Yiter,Xiter-1,Opened,Flagged,[$m|Acc],Mine);
	{ZeroedY,ZeroedX} ->
	    case lists:member({ZeroedY,ZeroedX},Opened) of
		true ->
		    form_xiter(Yiter,Xiter-1,Opened,Flagged,[$o|Acc],Mine);
		false ->
		    case lists:member({ZeroedY,ZeroedX},Flagged) of
			true ->
			    form_xiter(Yiter,Xiter-1,Opened,Flagged,[$f|Acc],Mine);
			false ->
			    form_xiter(Yiter,Xiter-1,Opened,Flagged,[$x|Acc],Mine)
		    end
	    end
    end.

%%--------------------------------------------------------------------
