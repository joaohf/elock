-module(elock_sqlapi).

-behaviour(sqlapi).

-export([
    authorize/4,
    connect_db/2,
    database/1,
    databases/1,
    tables/1,
    columns/2,
    terminate/2
]).

-export([
    select/3,
    update/4,
    fncall/3
]).

%%
%% sqlapi
%%

-record(state, {db}).

authorize(Username, HashedPassword, Hash, _Env) ->
    case <<"login">> == Username andalso sqlapi:password_hash(<<"pass">>, Hash) == HashedPassword of
        true -> {ok, #state{}};
        false -> {error, <<"Invalid login or password">>}
    end.

connect_db(Database, State) ->
    {ok, State#state{db = Database}}.

database(#state{db = DB}) -> DB.

databases(#state{}) -> [<<"elock">>].

terminate(_, _State) -> ok.

tables(#state{}) ->
    [<<"statistics">>, <<"timeout">>, <<"access_code">>, <<"service">>].

columns(Table, #state{}) -> columns(Table).

select(Table, Filter, _State) -> select(Table, Filter).

update(Table, Values, Conditions, _State) -> update(Table, Values, Conditions).

fncall(Function, Args, _State) ->
    ok = fncall(Function, Args),
    {ok, #{status => ok}}.

%%
%% Private
%%

columns(<<"statistics">>) ->
    [{nok_code, integer}, {ok_code, integer}];
columns(<<"timeout">>) ->
    [{timeout, integer}];
columns(<<"access_code">>) ->
    [{code, string}];
columns(<<"service">>) ->
    [{state, string}].

select(<<"statistics">>, _) ->
    {ok, Nok, Ok} = elock_statem:get_statistics(),
    [[{nok_code, Nok}, {ok_code, Ok}]];
select(<<"timeout">>, _) ->
    {ok, Tmo} = elock_statem:get_timeout(),
    [[{timeout, Tmo}]];
select(<<"access_code">>, _) ->
    {ok, Code} = elock_statem:get_code(),
    [[{code, Code}]];
select(<<"service">>, _) ->
    {ok, Service} = elock_statem:get_service(),
    [[{state, Service}]].

update(<<"timeout">>, #{<<"timeout">> := Tmo}, _) ->
    ok = elock_statem:set_timeout(Tmo),
    {ok, #{status => ok, affected_rows => 1}};
update(<<"service">>, #{<<"service">> := Service}, _) ->
    ok = elock_statem:set_service(Service),
    {ok, #{status => ok, affected_rows => 1}};
update(Table, _, _) ->
    {error, 1036, <<"cannot update non-updated ", Table/binary>>}.

fncall(<<"reset_code">>, []) ->
    ok = elock_statem:reset_code().
