%%%!------------------------------------------------------------------
%%% xmas -- 
%%%
%%% @Copyright:    
%%% @Creator:      Krishna Kumar Thokala
%%% @Date Created: 
%%% @Description:  
%%%-------------------------------------------------------------------
-module(xmas).

-rcs('$Id$').


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

-export([start_xmas/1]).
-export([shuffle/1,print_test_mails/0,send_me/0]).
%%--------------------------------------------------------------------
%% Include files
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% start_xmas - starts the xmas random email mapping 
%% start_xmas(MailFile) -> EmailMappings
%% MailFile = string() %string file name contain email addresses
%%                     %with new line and , separation
%% EmailMappings = [{Person,SantaFor}|..] %List of person and his
%%                                        %santa for
%% Person | SantaFor = {Id,Ref,Email}
%% Id = integer()
%% Ref = make_ref()
%% Email = bitstring()
%%--------------------------------------------------------------------
start_xmas(MailFile) ->
    case file:read_file(MailFile) of
	{ok,Binary} ->
	    Mails = binary:split(Binary,[<<",">>,<<"\n">>],[trim_all,global]),
	    %%Assigning unique numbers
	    Mapped = lists:foldl(fun(E,Acc) -> case Acc of
							   [] ->
							       [{1,make_ref(),E}|Acc];
							   [H|_T] ->
							       [{element(1,H)+1,make_ref(),E}|Acc] 
						       end end,[],Mails),
	    Shuffled = shuffle(Mapped),
	    mapping(Shuffled,[],Shuffled);
	Err ->
	    io:format("An Error reading file ~p,Err: ~p~n",[MailFile,Err])
    end.


mapping([],MapAcc,[]) ->
    %%io:format("Final Mapping:~p~n",[MapAcc]),
    MapAcc;
mapping([H|T],MapAcc,CompleteMap) ->
    Len = length(CompleteMap),
    UNum = erlang:unique_integer([positive]) + 100,
    case Len of
	0 ->
	    [E|[]] = CompleteMap,
	    mapping(T,[{H,E}|MapAcc],[]);
	2 ->
	    %%Check the last two persons has same names
	    Tail = lists:last(T),
	    H1 = hd(CompleteMap),
	    HTail = lists:last(CompleteMap),
	    %%io:format("Got sameH:~p,T:~p,CompMap:~p~n",[H,T,CompleteMap]),
	    case [lists:reverse(CompleteMap)||Tail =:= HTail orelse H=:=hd(CompleteMap)] of
		[] ->
		    mapping([],[{H,hd(CompleteMap)}]++[{Tail,HTail}]++MapAcc,[]);
		_NewMap ->
		    mapping([],[{H,HTail}]++[{Tail,H1}]++MapAcc,[])
	    end;
	Len when Len>0 ->
	    MemPos = case UNum rem Len of
			 N when N>0 ->
			     N;
			 0 ->
			     erlang:unique_integer([positive]) rem Len + 1
		     end,
	    case lists:nth(MemPos,CompleteMap) of
		H ->
		    io:format("retrying ~p~n",[H]),
		    mapping([H|T],MapAcc,CompleteMap);
		Member when is_tuple(Member)->
		    %%io:format("~p -> ~p~n",[H,Member]),
		    RemT = lists:delete(Member,CompleteMap),
		    mapping(T,[{H,Member}|MapAcc],RemT);
		_ ->
		    ok
	    end;
	_ ->
	    io:format("Cannot pair ~p~n",[H]),
	    mapping([],MapAcc,CompleteMap)
    end.

shuffle(List) ->
    shuffle_it(List,[]).

shuffle_it([H|[]],Acc) ->
    %%io:format("Final shuffle length:~p~n",[length([H|Acc])]),
    [H|Acc];
shuffle_it(List,Acc) ->
    L=length(List),
    EPos = erlang:unique_integer([positive]) rem L + 1,
    Elem = lists:nth(EPos,List),
    shuffle_it(lists:delete(Elem,List),[Elem|Acc]).

%%--------------------------------------------------------------------
%% Main API
%% print_test_mails - Write Email mappings to a file and send mails
%% print_test_mails() -> ok
%%--------------------------------------------------------------------
print_test_mails() ->  
    OldState = start_xmas("emails1"),
    Mappings = [print_test_mails(binary_to_list(element(3,element(2,Each))),binary_to_list(element(3,element(1,Each))))||Each<-OldState],
    file:write_file("mappings",Mappings).

print_test_mails(SantaFor,ToMailId) ->
    MailCmd = "mail -s"++" \"NNTO: Hey this is final,you are santa for "++SantaFor++"\" "++ToMailId,
    %%io:format("~p~n",[MailCmd]),
    os:cmd(MailCmd),
    MailCmd.
