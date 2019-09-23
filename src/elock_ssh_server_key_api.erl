%% A ssh server key api callbacks just to bypass default ssh files
%% 
%% In this example I don't want to use the default ssh_file module
%% which oblies to have ssh files.
%%
%% See: http://erlang.org/doc/man/ssh_file.html

-module(elock_ssh_server_key_api).

-behaviour(ssh_server_key_api).

-export([host_key/2, is_auth_key/3]).

host_key('ssh-rsa', DaemonOptions) ->
    Key = public_key:generate_key({rsa, 2048, 17}),
    {ok, Key}.

%% disable authentication using keys
is_auth_key(_PublicUserKey, _User, _DaemonOptions) -> false.