-module(tag_check).

%% API exports
-export([main/1, get_repos/1]).

%%====================================================================
%% API functions
%%====================================================================

-define(GQ_LOGGEDIN, <<"query { viewer { login }}">>).

-define(GQUrl, "https://api.github.com/graphql").

main([]) -> main([<<"K2InformaticsGmbH">>, <<"dderl">>, <<"master">>]);
main([O, R, T]) when is_list(O) -> main([list_to_binary(O), R, T]);
main([O, R, T]) when is_list(R) -> main([O, list_to_binary(R), T]);
main([O, R, T]) when is_list(T) -> main([O, R, list_to_binary(T)]);
main([Owner, Repo, Tag])
	when is_binary(Owner), is_binary(Repo), is_binary(Tag)
->
	inets:start(),
	ssl:start(),
	Priv = code:priv_dir(tag_check),
	io:format("~p:~p Priv ~s~n", [?MODULE, ?LINE, Priv]),
	{ok, GqlGetTextFile} =
		file:read_file(
			filename:join([
				Priv, "..", "..", "..", "..", "lib", "tag_check", "priv",
				"get_text_file.gql"
			])
	),
	{ok, GqlIsFork} =
		file:read_file(
			filename:join([
				Priv, "..", "..", "..", "..", "lib", "tag_check", "priv",
				"is_fork.gql"
			])
	),
	GitToken = try
		NetRcFile = case os:type() of
			{win32, nt} ->
				filename:join(
					os:getenv("HOMEDRIVE") ++ os:getenv("HOMEPATH"), "_netrc"
				)
		end,
		io:format("~p:~p netrc ~s~n", [?MODULE, ?LINE, NetRcFile]),
		{ok, NetRcBin} = file:read_file(NetRcFile),
		{match, [Token]} =
			re:run(NetRcBin, ".*login ([^\s]*)",
					[dotall, {capture, [1], list}]),
		Token
	catch _:Exception ->
		error({bad_gittoken, Exception})
	end,
	recursive_check_rebar_config(
		#{token => GitToken,
		 gqls => #{getTextFile => GqlGetTextFile, isFork => GqlIsFork}
		}, [{Owner, Repo, Tag}]).

recursive_check_rebar_config(_Ctx, []) ->
	io:format("~p:~p -------- F I N I S H E D --------~n", [?MODULE, ?LINE]);
recursive_check_rebar_config(
	#{token := GitToken,
	  gqls := #{getTextFile := GqlGetTextFile, isFork := GqlIsFork}
	} = Ctx, [{Owner, Repo, Tag} | Rest]
) ->
	NewRepos = case is_fork(?GQUrl, GitToken, GqlIsFork, Owner, Repo) of
		true ->
			io:format(
				"~p:~p SKIP ~p is a fork no more recurse~n",
				[?MODULE, ?LINE, {Owner, Repo}]
			),
			Rest;
		_ ->
			io:format("~p:~p checking ~p~n", [?MODULE, ?LINE, {Owner, Repo, Tag}]),
			case read_file(
				?GQUrl, GitToken, GqlGetTextFile, Owner, Repo,
				<<Tag/binary,":rebar.config">>
			) of
				error -> Rest;
				Data ->
					Term = binstr_to_term(Data),
					ExtraRepos = get_repos(Term),
					lists:usort(Rest ++ ExtraRepos)
			end
	end,
	recursive_check_rebar_config(Ctx, NewRepos).

get_repos(Skip)
	when is_atom(Skip); Skip == []; is_integer(Skip)
-> [];
get_repos([H|T]) -> get_repos(H) ++ get_repos(T);
get_repos({App, {git, Url, Branch}}) when Branch == master ->
	io:format(
		"~p:~p ERROR ~p(~s) is from master branch~n",
		[?MODULE, ?LINE, App, Url]
	),
	[];
get_repos({App, {git, Url, {branch, Branch}}}) ->
	io:format(
		"~p:~p ERROR ~p(~s) is from ~p branch~n",
		[?MODULE, ?LINE, App, Url, Branch]
	),
	[];
get_repos({_, {git, Url, {tag, Tag}}}) ->
	UrlNoGit = re:replace(Url, "\.git$", "", [{return, binary}]),
	{match, [Owner, Repo]} = re:run(
		UrlNoGit, "/([^/]+)/([^/]+$)", [{capture, [1,2], binary}]
	),
	[{Owner, Repo, list_to_binary(Tag)}];
get_repos(Tuple) when is_tuple(Tuple) ->
	get_repos(tuple_to_list(Tuple)).

%%====================================================================
%% Internal functions
%%====================================================================

-ifdef(CONDOLE).

curl -H "Authorization: bearer `cat ~/_netrc | awk '/login.*/ {print $2}'`" \
	-X POST -d "@viewer_login.json" https://api.github.com/graphql

curl -H "Authorization: bearer `cat ~/_netrc | awk '/login.*/ {print $2}'`" \
	-X POST -d "@text_file.json" https://api.github.com/graphql

ok.

-endif.

read_file(GQUrl, Token, GqlBin, Owner, Repo, File) ->
	{ok, {_Status, _Headers, Body}} = httpc:request(
		post,
		{
			GQUrl,
			[{"Authorization", "bearer " ++ Token},
				{"User-Agent", "TagChecker/1.0"}],
			"application/json",
			jsx:encode(
				#{
					query => GqlBin,
					variables => #{
						owner => Owner,
						repo => Repo,
						path => File
					}
				}
			)
		}, [], [{body_format, binary}]
	),
	case jsx:decode(Body, [return_maps]) of
		#{<<"data">> :=
			#{<<"repository">> :=
				#{<<"object">> :=
					#{<<"text">> := Data}}}} -> Data;
		Error ->
			io:format(
				"~p:~p ERROR reading ~p~n~p~n",
				[?MODULE, ?LINE, {Owner, Repo, File}, Error]
			),
			error
	end.

is_fork(GQUrl, Token, GqlBin, Owner, Repo) ->
	{ok, {_Status, _Headers, Body}} = httpc:request(
		post,
		{
			GQUrl,
			[{"Authorization", "bearer " ++ Token},
				{"User-Agent", "TagChecker/1.0"}],
			"application/json",
			jsx:encode(
				#{
					query => GqlBin,
					variables => #{
						owner => Owner,
						repo => Repo
					}
				}
			)
		}, [], [{body_format, binary}]
	),
	case jsx:decode(Body, [return_maps]) of
		#{<<"data">> :=
			#{<<"repository">> :=
				#{<<"isFork">> := IsFork}}} -> IsFork;
		Error ->
			io:format(
				"~p:~p ERROR reading ~p~n~p~n",
				[?MODULE, ?LINE, {Owner, Repo}, Error]
			),
			error
	end.

binstr_to_term(BinTermStr) when is_binary(BinTermStr) ->
	binstr_to_term(binary_to_list(BinTermStr));
binstr_to_term(TermStr) ->
	{ok, ErlTokensAll, _} = erl_scan:string(TermStr),
	[begin
  		{ok,ErlAbsForm} = erl_parse:parse_exprs(ErlTokens),
  		{value, Value,_} = erl_eval:exprs(ErlAbsForm, []),
		Value
	 end || ErlTokens <- split_dot(ErlTokensAll)].

split_dot(Tokens) -> split_dot(Tokens, [[]]).
split_dot([], [[]|AccRest]) -> lists:reverse(AccRest);
split_dot([], [Acc|AccRest]) -> lists:reverse([lists:reverse(Acc)|AccRest]);
split_dot([{dot,_}=D|Rest], [Acc|AccRest]) ->
	split_dot(Rest, [[],lists:reverse([D|Acc])|AccRest]);
split_dot([T|Rest], [Acc|AccRest]) ->
	split_dot(Rest, [[T|Acc]|AccRest]).