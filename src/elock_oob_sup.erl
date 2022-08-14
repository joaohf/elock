-module(elock_oob_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Sql = #{
        id => elock_sql_oob,
        start => {elock_sql_oob, start_link, []},
        restart => permanent,
        type => worker,
        shutdown => 5000,
        modules => [elock_sql_oob]
    },
    Ssh = #{
        id => elock_ssh_oob,
        start => {elock_ssh_oob, start_link, []},
        restart => permanent,
        type => worker,
        shutdown => 5000,
        modules => [elock_ssh_oob]
    },
    SupFlags = #{
        strategy => one_for_one,
        intensity => 10,
        period => 1
    },
    ChildSpecs = [Sql, Ssh],
    {ok, {SupFlags, ChildSpecs}}.
