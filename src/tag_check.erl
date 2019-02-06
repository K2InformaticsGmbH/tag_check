-module(tag_check).

%% API exports
-export([main/1, split_dot/1]).

%%====================================================================
%% API functions
%%====================================================================

-define(GQ_LOGGEDIN, <<"query { viewer { login }}">>).

-define(GQUrl, "https://api.github.com/graphql").

main([]) ->
	inets:start(),
	ssl:start(),
	Priv = code:priv_dir(tag_check),
	io:format("~p:~p Priv ~s~n", [?MODULE, ?LINE, Priv]),
	{ok, GqlQueryBin} =
		file:read_file(
			filename:join([
				Priv, "..", "..", "..", "..", "lib", "tag_check", "priv",
				"get_text_file.gql"
			])
	),
	GitToken = try
		NetRcFile = case os:type() of
			{win32, nt} ->
				filename:join(
					os:getenv("HOMEDRIVE") ++ os:getenv("HOMEPATH"), "_netrc"
				)
		end,
		{ok, NetRcBin} = file:read_file(NetRcFile),
		{match, [Token]} =
			re:run(NetRcBin, ".*login ([^\s]*)",
					[dotall, {capture, [1], list}]),
		Token
	catch _:Exception ->
		error({bad_gittoken, Exception})
	end,
	Data = read_file(
		?GQUrl, GitToken, GqlQueryBin, <<"K2InformaticsGmbH">>, <<"dderl">>,
		<<"master:rebar.config">>
	),
	Term = binstr_to_term(Data),
	io:format("~p:~p ~p~n", [?MODULE, ?LINE, Term]).

%%====================================================================
%% Internal functions
%%====================================================================
%
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
	#{<<"data">> :=
		#{<<"repository">> :=
			#{<<"object">> :=
				#{<<"text">> := Data}}}} = jsx:decode(Body, [return_maps]),
	Data.

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