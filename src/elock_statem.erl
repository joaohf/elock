-module(elock_statem).
-behaviour(gen_statem).
-define(NAME, code_lock).

-export([start_link/1]).
-export([button/1]).
-export([init/1,callback_mode/0,terminate/3]).
-export([locked/3,open/3]).

-export([get_statistics/0, get_timeout/0, set_timeout/1, get_code/0, get_service/0, set_service/1, reset_code/0]).

get_statistics() ->
    {ok, 10, 2}.

get_timeout() ->
    {ok, 10000}.

set_timeout(Tmo) -> ok.

get_code() ->
    {ok, <<"12345">>}.

get_service() ->
    {ok, <<"inservice">>}.

set_service(Service) -> ok.

reset_code() -> ok.

start_link(Code) ->    
    gen_statem:start_link({local,?NAME}, ?MODULE, Code, []).

button(Button) ->
    gen_statem:cast(?NAME, {button,Button}).

init(Code) ->
    io:format("code: ~p", [Code]),
    do_lock(),
    Data = #{code => Code, length => length(Code), buttons => []},
    {ok, locked, Data}.

callback_mode() ->
    state_functions.

locked(
  cast, {button,Button},
  #{code := Code, length := Length, buttons := Buttons} = Data) ->
    NewButtons =
        if
            length(Buttons) < Length ->
                Buttons;
            true ->
                tl(Buttons)
        end ++ [Button],
    if
        NewButtons =:= Code -> % Correct
	    do_unlock(),
            {next_state, open, Data#{buttons := []},
             [{state_timeout,10000,lock}]}; % Time in milliseconds
	true -> % Incomplete | Incorrect
            {next_state, locked, Data#{buttons := NewButtons}}
    end.

open(state_timeout, lock,  Data) ->
    do_lock(),
    {next_state, locked, Data};
open(cast, {button,_}, Data) ->
    {next_state, open, Data}.

do_lock() ->
    io:format("Lock~n", []).
do_unlock() ->
    io:format("Unlock~n", []).

terminate(_Reason, State, _Data) ->
    State =/= locked andalso do_lock(),
    ok.