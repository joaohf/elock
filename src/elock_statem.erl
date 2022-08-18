-module(elock_statem).
-behaviour(gen_statem).
-define(NAME, code_lock).

-export([start_link/1, stop/0]).
-export([button/1]).
-export([init/1, callback_mode/0, terminate/3]).
-export([locked/3, open/3]).

-export([
    get_statistics/0,
    get_timeout/0,
    set_timeout/1,
    get_code/0,
    get_service/0,
    set_service/1,
    reset_code/0,
    is_locked/0
]).

-include_lib("kernel/include/logger.hrl").

%% @doc Returns number of lock times and tries.
%% @TODO TBI
%% @end
get_statistics() ->
    {ok, 10, 2}.

%% @doc Returns the current timeout value.
%% @TODO TBI
%% @end
get_timeout() ->
    {ok, 10000}.

%% @doc Configures a new timeout value.
%% @TODO TBI
%% @end
set_timeout(_Tmo) -> ok.

%% @doc Returns the current code.
%% @TODO TBI
%% @end
get_code() ->
    {ok, <<"12345">>}.

%% @doc Get the current service value.
%% @TODO TBI
%% @end
get_service() ->
    {ok, <<"inservice">>}.

%% @doc Configure a new service value.
%% @TODO TBI
%% @end
set_service(Service) -> ok.

%% @doc Reset code to the initial well know value.
%% @TODO TBI
%% @end
reset_code() -> ok.

%% @doc Check if the lock is locked.
%% @end
is_locked() ->
    gen_statem:call(?NAME, is_locked).

%% @doc Sends a digit to lock.
%% @end
button(Button) ->
    gen_statem:cast(?NAME, {button, Button}).

start_link(Code) ->
    gen_statem:start_link({local, ?NAME}, ?MODULE, Code, []).

stop() ->
    gen_statem:stop(?NAME).

init(Code) ->
    logger:set_module_level(?MODULE, debug),
    do_lock(),
    Data = #{code => Code, length => length(Code), buttons => []},
    {ok, locked, Data}.

callback_mode() ->
    state_functions.

locked(
    cast,
    {button, Button},
    #{code := Code, length := Length, buttons := Buttons} = Data
) ->
    NewButtons =
        if
            length(Buttons) < Length ->
                Buttons;
            true ->
                tl(Buttons)
        end ++ [Button],
    if
        % Correct
        NewButtons =:= Code ->
            do_unlock(),
            {next_state, open, Data#{buttons := []},
                % Time in milliseconds
                [{state_timeout, 10000, lock}]};
        % Incomplete | Incorrect
        true ->
            {next_state, locked, Data#{buttons := NewButtons}}
    end;
locked({call, From}, is_locked, State) ->
    {keep_state, State, [{reply, From, yes}]}.

open(state_timeout, lock, Data) ->
    do_lock(),
    {next_state, locked, Data};
open(cast, {button, _}, Data) ->
    {next_state, open, Data};
open({call, From}, is_locked, State) ->
    {keep_state, State, [{reply, From, no}]}.

do_lock() ->
    ?LOG_DEBUG("Lock~n", []).
do_unlock() ->
    ?LOG_DEBUG("Unlock~n", []).

terminate(_Reason, State, _Data) ->
    State =/= locked andalso do_lock(),
    ok.
