%%%-------------------------------------------------------------------
%%% @author  <>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 27 Aug 2011 by  <>
%%%-------------------------------------------------------------------
-module(cowboy_http).
-behaviour(cowboy_http_handler).
%% API
-export([init/3, handle/2,terminate/2]).
-include_lib("cowboy/include/http.hrl").
-define(JSON_HEADER, [{<<"Content-Type">>, <<"application/json">>}]).
%%%===================================================================
%%% API
%%%===================================================================

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req,State) ->
    {Path,_} = cowboy_http_req:path(Req),
    io:format("Path for request is ~p~n",[Path]),
    {ok, Req2} = handle('GET',Path,Req),
    {ok,Req2,State}.

handle('GET', [], Req) ->
    FilePath = code:priv_dir(erlstats_web) ++ "/index.html",
    {ok,Body} = file:read_file(FilePath),
    cowboy_http_req:reply(200, [{<<"Content-Type">>, <<"text/html">>}],Body,Req);

handle('GET',[<<"all">>,<<"json">>],Req) ->
    io:format("getting all stats~n"),
    Stats = lists:map(fun(Stat) ->
			      convert_stat_value(Stat)
		      end, erlstats:get_all_stats()),
    case Stats of
	[] ->
	    cowboy_http_req:reply(200,?JSON_HEADER, jsx:term_to_json([{<<"result">>,<<"no stats registered">>}]),Req);
	_ ->
	    cowboy_http_req:reply(200,?JSON_HEADER, jsx:term_to_json(Stats),Req)
    end;

handle('GET',[StatName, <<"json">>],Req) ->
    Stats = convert_stat_value(erlstats:get_stat(name_to_atom(StatName))),
    cowboy_http_req:reply(200,?JSON_HEADER, jsx:term_to_json([{list_to_binary(StatName), Stats}]),Req);

handle('GET', [<<"increment">>,StatName],Req) ->
    erlstats:increment_stat(name_to_atom(StatName)),
    handle('GET',[],Req#http_req{path = []});

handle('GET', [<<"increment">>,StatName,Count],Req) ->
    erlstats:increment_stat(name_to_atom(StatName),Count),
    handle('GET',[],Req#http_req{path = []});

handle('GET', [<<"register">>,StatName],Req) ->
    erlstats:register_stat({name_to_atom(StatName),counter}),
    handle('GET',[],Req#http_req{path = []});

handle('GET', [<<"register">>,StatName,<<"value">>],Req) ->
    erlstats:register_stat({name_to_atom(StatName),value}),
    handle('GET',[],Req#http_req{path = []});

handle('GET', [<<"register">>,StatName,<<"counter">>],Req) ->
    erlstats:register_stat({name_to_atom(StatName),counter}),
    handle('GET',[],Req#http_req{path = []});

handle('GET', [<<"update">>,StatName,NewValue],Req) ->
    erlstats:update_stat(name_to_atom(StatName),NewValue),
    handle('GET',[],Req#http_req{path = []});

handle('GET', [<<"reset">>,StatName],Req) ->
    erlstats:reset_stat(name_to_atom(StatName)),
    handle('GET',[],Req#http_req{path = []});

handle('GET', [<<"destroy">>,StatName],Req) ->
    erlstats:destroy_stat(name_to_atom(StatName)),
    handle('GET',[],Req#http_req{path = []});

handle(_,_,Req) ->
    cowboy_http_req:reply(200,[{<<"Content-Type">>, <<"text/plain">>}], "Page not found\nErlstats Web",Req).

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
convert_stat_value({Name, Value}) when is_atom(Value) ->
    {Name, atom_to_binary(Value, latin1)};
convert_stat_value({Name, Value}) when is_list(Value) ->
    {Name, list_to_binary(Value)};
convert_stat_value({Name, {H,M,S,MS}}) ->
    {Name, list_to_binary(integer_to_list(H)++":"++integer_to_list(M)++":"++integer_to_list(S)++"-"++integer_to_list(MS))};
convert_stat_value({Name, Value}) ->
    {Name, Value}.

name_to_atom(Name) when is_list(Name) ->
    list_to_atom(Name);
name_to_atom(Name) when is_binary(Name) ->
    Name;
name_to_atom(Name) when is_atom(Name) ->
    atom_to_binary(Name,latin1).
