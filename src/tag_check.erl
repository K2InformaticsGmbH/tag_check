-module(tag_check).
-include("token.hrl").

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
	Data = read_file(?GQUrl, ?Token, "dderl", "K2InformaticsGmbH", "master", "rebar.config"),
	Term = binstr_to_term(Data),
	io:format("~p:~p ~p~n", [?MODULE, ?LINE, Term]).

%%====================================================================
%% Internal functions
%%====================================================================

read_file(GQUrl, Token, Repo, Owner, Branch, PathFile) ->
		{ok, {_Status, _Headers, Body}} = httpc:request(
			post,
			{GQUrl,
			[{"Authorization", "bearer " ++ Token}, {"User-Agent", "TagChecker/1.0"}],
			"application/json",
			jsx:encode(#{query => 
				list_to_binary(
						"query {"
							" repository(name: \""++Repo++"\", owner: \""++Owner++"\") {"
								" object(expression: \""++Branch++":"++PathFile++"\") {"
									"... on Blob { text }"
								"}"
							"}"
						"}")})
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