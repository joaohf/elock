%%%-------------------------------------------------------------------
%% @doc elock public API
%% @end
%%%-------------------------------------------------------------------

-module(elock_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    elock_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
