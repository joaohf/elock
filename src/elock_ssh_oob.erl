-module(elock_ssh_oob).

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

-define(DEFAULT_SSHD_PORT, 4050).
-define(DEFAULT_SSHD_PASSWORDS, [{"admin", "elock"}]).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    Sshd = application:get_env(sshd),
    State = get_sshd_config(Sshd),
    {ok, State, {continue, sshd}}.

handle_continue(sshd, #{port := Port} = State) ->
    Opts = get_sshd_options(State),
    {ok, Pid} = ssh:daemon(Port, Opts),
    link(Pid),
    {noreply, State#{pid => Pid}, hibernate}.

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

get_sshd_config(undefined) ->
    Passwords = ?DEFAULT_SSHD_PASSWORDS,
    #{
        port => ?DEFAULT_SSHD_PORT,
        passwords => Passwords
    };
get_sshd_config(Sshd) ->
    Port = proplists:get_value(port, Sshd, ?DEFAULT_SSHD_PORT),
    Passwords = proplists:get_value(passwords, Sshd, ?DEFAULT_SSHD_PASSWORDS),
    #{
        port => Port,
        passwords => Passwords
    }.

get_sshd_options(#{passwords := Passwords}) ->
    [
        {key_cb, elock_ssh_server_key_api},
        {preferred_algorithms, [{public_key, ['ssh-rsa']}]},
        {shell, fun(User, PeerAddr) -> elock_ssh_cli:start_elock_shell(User, PeerAddr) end},
        {user_passwords, Passwords}
    ].
