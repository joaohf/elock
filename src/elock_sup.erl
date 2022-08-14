%%%-------------------------------------------------------------------
%% @doc elock top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(elock_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    ElockStatem = #{
        id => elock_statem,
        start => {elock_statem, start_link, ["1234"]},
        restart => permanent,
        type => worker,
        shutdown => 5000,
        modules => [elock_statem]
    },
    OobSup = #{
        id => elock_oob_sup,
        start => {elock_oob_sup, start_link, []},
        restart => permanent,
        type => supervisor,
        shutdown => infinity,
        modules => [elock_oob_sup]
    },
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 1
    },
    ChildSpecs = [ElockStatem, OobSup],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
