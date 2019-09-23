%% A base ssh CLI
%%
%% I've borrow most of the code from here:
%% https://github.com/erlang/otp/blob/master/lib/ssh/examples/ssh_sample_cli.erl
-module(elock_ssh_cli).

-export([start_elock_shell/2]).

%% cli command functions
-export([cli_exit/0,
         cli_help/0,
         cli_get_timeout/0,
         cli_set_timeout/1,
         cli_get_statistics/0,
         cli_get_code/0,
         cli_get_service/0,
         cli_set_service/1,
         cli_reset_code/0]).

%% our_routines
our_routines() ->
    [
        {"help",           cli_help,           "            help text"},
        {"exit",           cli_exit,           "            exit application"},
        {"set_timeout",    cli_set_timeout,    "<int>       set timeout <int>"},
        {"get_timeout",    cli_get_timeout,    "            get timeout"},
        {"get_statistics", cli_get_statistics, "            get statistics"},
        {"get_code",       cli_get_code,       "            get lock code"},
        {"get_service",    cli_get_service,    "            get service"},
        {"set_service",    cli_set_service,    "<string>    set service"},
        {"reset_code",     cli_reset_code,     "            reset default code"}
    ].

%% the longest common prefix of two strings
common_prefix([C | R1], [C | R2], Acc) ->
    common_prefix(R1, R2, [C | Acc]);
common_prefix(_, _, Acc) ->
    lists:reverse(Acc).

%% nthtail as in lists, but no badarg if n > the length of list
nthtail(0, A) -> A;
nthtail(N, [_ | A]) -> nthtail(N-1, A);
nthtail(_, _) -> [].

%% longest prefix in a list, given a prefix
longest_prefix(List, Prefix) ->
    case [A || {A, _, _} <- List, lists:prefix(Prefix, A)] of
	[] ->
	    {none, List};
	[S | Rest] ->
	    NewPrefix0 =
		lists:foldl(fun(A, P) ->
				    common_prefix(A, P, [])
			    end, S, Rest),
	    NewPrefix = nthtail(length(Prefix), NewPrefix0),
	    {prefix, NewPrefix, [S | Rest]}
    end.			

%% our expand function (called when the user presses TAB)
expand([$  | _]) ->
    {no, "", []};
expand(RevBefore) ->    
    Before = lists:reverse(RevBefore),
    case longest_prefix(our_routines(), Before) of
	{prefix, P, [_]} ->
	    {yes, P ++ " ", []};
	{prefix, "", M} ->
	    {yes, "", M};
	{prefix, P, _M} ->
	    {yes, P, []};
	{none, _M} ->
	    {no, "", []}
    end.

start_elock_shell(User, Peer) ->
    spawn(fun() ->
		  io:setopts([{expand_fun, fun(Bef) -> expand(Bef) end}]),
		  io:format("Enter command or `help`\n"),
		  put(user, User),
		  put(peer_name, Peer),
		  our_shell_loop()
	  end).

our_shell_loop() ->
    % Read
    Line = io:get_line("elock> "),
    % Eval
    Result = eval_cli(Line),
    % Print
    io:format("---> ~p\n", [Result]),
    case Result of
	    done -> 
	        exit(normal);
	    _ -> 
	        our_shell_loop()
    end.

%% translate a command to a function
command_to_function(Command) ->
    case lists:keysearch(Command, 1, our_routines()) of
	{value, {_, Proc, _}} -> 
	    Proc;
	false -> 
	    unknown_cli
    end.

%% evaluate a command line
eval_cli(Line) ->
    case string:tokens(Line, " \n") of
	[] -> [];
	[Command | ArgStrings] ->
	    Proc = command_to_function(Command),
	    case fix_args(ArgStrings) of
		{ok, Args} ->
		    case catch apply(?MODULE, Proc, Args) of
			{'EXIT', Error} ->
			    {error, Error}; % wrong_number_of_arguments};
			Result ->
			    Result
		    end;
		Error ->
		    Error
	    end
    end.

%% make command arguments to integers
fix_args(ArgStrings) ->
    case catch [list_to_integer(A) || A <- ArgStrings] of
	{'EXIT', _} ->
	    {error, only_integer_arguments};
	Args ->
	    {ok, Args}
    end.

%%
%% CLI Commands
%%

help_str(L) ->
    help_str(L, []).
help_str([], Acc) ->
    lists:sort(Acc);
help_str([{CommandName, _, HelpS} | Rest], Acc) ->
    C = string:left(CommandName, 20),
    help_str(Rest, [[C, " ", HelpS, $\n] | Acc]).

cli_help() ->
    HelpString = ["CLI elock\n" | help_str(our_routines())],
    io:format("~s\n", [HelpString]).

cli_exit() ->
    done.

cli_set_timeout(Tmo) ->
    elock_statem:set_timeout(Tmo).

cli_get_timeout() ->
    elock_statem:get_timeout().

cli_get_statistics() ->
    elock_statem:get_statistics().

cli_get_code() ->
    elock_statem:get_code().

cli_get_service() ->
    elock_statem:get_service().

cli_set_service(Service) ->
    elock_statem:set_service(Service).

cli_reset_code() ->
    elock_statem:reset_code().