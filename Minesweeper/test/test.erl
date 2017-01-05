-module(test).
-export([start_suite/0]).
-export([start_stop_test/0,load_test/1,out_of_bounds_test/3,manual_flagging/0,
	open_flagged/0,flag_to_unflag/0,flag_opened/0,reset/0,
	repeated_flagging/0,invalid_input/1]).

start_suite() ->
    TestCases = [
		 {?MODULE,start_stop_test,[]},
		 {?MODULE,load_test,["xxmm,mmxx,xmxm"]},
		 {?MODULE,out_of_bounds_test,["xxmm,mmxx,xmxm",4,4]},
		 {?MODULE,manual_flagging,[]},
		 {?MODULE,open_flagged,[]},
		 {?MODULE,flag_to_unflag,[]},
		 {?MODULE,flag_opened,[]},
		 {?MODULE,reset,[]},
		 {?MODULE,repeated_flagging,[]}
		 %%{?MODULE,max_flags_test,[]},
		 %%{?MODULE,unflag_and_reflag,[]},
		 %%{?MODULE,no_mines,[]},
		 %%{?MODULE,simple_win_test,[]},
		 %%{?MODULE,open_flagged,[]}
		],
    NegTestCases = [
		    {?MODULE,invalid_input,["xxmm,mmxx,fgd"]}
		   ],
    %%Stop erlier execution of game
    newsweep:stop(),
    case catch {[erlang:apply(M,F,A)||{M,F,A}<-TestCases],
    [erlang:apply(M,F,A)||{M,F,A}<-NegTestCases]} of
	{L,R} when is_list(L),
		   is_list(R) ->
	    io:format("~n ----> Result: Succcessful execution of tests <----- ~n");
	_ ->
	    io:format("~n ----> Result: UnSucccessful execution of tests <----- ~n")
    end.
    
start_stop_test() ->
    {ok,_Pid} = newsweep:start(),
    ok = newsweep:stop(),
    {ok,_Pid1} = newsweep:start().

invalid_input(String) ->
    {invalid_input,String} = newsweep:load(String).

load_test(String) ->
    {mines,_List} =  newsweep:load(String).

out_of_bounds_test(String,Ybound,Xbound) ->
    {mines,_List} =  newsweep:load(String),
    {invalid_bound} = newsweep:o(Ybound,Xbound).

manual_flagging() ->
    load_test("xxmm,mmxx,xmxm"),
    {ok,["xxxx","xxxx","xxfx"]} = newsweep:f(2,2).

open_flagged() ->
    load_test("xxmm,mmxx,xmxm"),
    {ok,["xxxx","xxxx","xxfx"]} = newsweep:f(2,2),
    {ok,["xxxx","xxxx","xxox"]} = newsweep:o(2,2).

flag_to_unflag() ->
    load_test("xxmm,mmxx,xmxm"),
    {ok,["xxxx","xxxx","xxfx"]} = newsweep:f(2,2),
    {ok,["xxxx","xxxx","xxxx"]} = newsweep:uf(2,2).

flag_opened() ->
    load_test("xxmm,mmxx,xmxm"),
    {ok,["xxxx","xxxx","xxfx"]} = newsweep:f(2,2),
    {ok,["xxxx","xxxx","xxox"]} = newsweep:o(2,2).

repeated_flagging() ->
    String = "xxmm,mmxx,xmxm",
    MineCount = lists:foldl(fun($m,Acc) -> Acc+1;
			       (_,Acc) -> Acc end,0,String),
    load_test(String),
    %%Randomly, I am trying flag a coordinate more number of times than 
    %%the mine count
    ExtrapolatedCount = MineCount + 4,
    ExtrapolatedCount = length([E||E<-lists:seq(1,ExtrapolatedCount),{ok,["xxxx","xxxx","xxfx"]} == newsweep:f(2,2)]).

reset() ->
    {reset,_ListOfRows} =  newsweep:reset().

