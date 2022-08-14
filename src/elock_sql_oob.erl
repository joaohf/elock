-module(elock_sql_oob).

-behaviour(gen_server).

-define(SERVER, ?MODULE).

-export([start_link/0]).

-export([
    init/1,
    handle_continue/2,
    handle_call/3,
    handle_cast/2,
    terminate/2,
    handle_info/2,
    code_change/3
]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%
%% gen_server
%%

init([]) ->
    {ok, #{}, {continue, sqlapi}}.

handle_continue(sqlapi, State) ->
    {ok, Pid} = start_sqlapi_and_link(),
    {noreply, State#{pid => Pid}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

start_sqlapi_and_link() ->
    Env = #{
        port => 4406,
        listener_name => elock_sql_oob,
        handler => elock_sqlapi,
        trivial => false
    },
    _ =
        case sqlapi:start_server(Env) of
            {ok, Pid} -> Pid;
            {error, {already_started, Pid}} -> Pid
        end,

    link(Pid),

    {ok, Pid}.
