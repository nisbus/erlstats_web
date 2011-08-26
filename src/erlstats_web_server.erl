%%%-------------------------------------------------------------------
%%% @author  nisbus <nisbus@gmail.com>
%%% @copyright (C) 2011, 
%%% @doc
%%%
%%% @end
%%% Created : 24 Aug 2011 by nisbus <nisbus@gmail.com>
%%%-------------------------------------------------------------------
-module(erlstats_web_server).

-behaviour(gen_server).

%% API
-export([start_link/1,stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Port) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Port], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Port]) ->
    process_flag(trap_exit,true),
    misultin:start_link([{port, Port}, 
			 {loop, fun(Req) -> handle(Req) end}]),
    erlang:monitor(process, misultin),
    {ok, []}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
stop() ->
    misultin:stop().

handle(Req) ->
    handle(Req:get(method), Req:resource([lowercase, urldecode]), Req).

handle('GET', [],Req) ->
    Req:file(code:priv_dir(erlstats_web) ++ "/index.html");

handle('GET',["all","json"],Req) ->
    Stats = lists:map(fun(Stat) ->
			      convert_stat_value(Stat)
			   end, erlstats:get_all_stats()),
    case Stats of
	[] ->
	    Req:ok([{"Content-Type", "application/json"}], jsx:term_to_json([{<<"result">>,<<"no stats registered">>}]));
	_ ->

	    Req:ok([{"Content-Type", "application/json"}], jsx:term_to_json(Stats))
    end;

handle('GET',[StatName, "json"],Req) ->
    Stats = convert_stat_value(erlstats:get_stat(name_to_atom(StatName))),
    Req:ok([],jsx:term_to_json([{list_to_binary(StatName), Stats}]));

handle('GET', ["increment",StatName],Req) ->
    erlstats:increment_stat(name_to_atom(StatName)),
    Req:ok([],[]);

handle('GET', ["increment",StatName,Count],Req) ->
    erlstats:increment_stat(name_to_atom(StatName),Count),
    Req:ok([],[]);

handle('GET', ["register",StatName],Req) ->
    erlstats:register_stat({name_to_atom(StatName),counter}),
    Req:ok([],[]);

handle('GET', ["register",StatName,"value"],Req) ->
    erlstats:register_stat({name_to_atom(StatName),value}),
    Req:ok([],[]);

handle('GET', ["register",StatName,"counter"],Req) ->
    erlstats:register_stat({name_to_atom(StatName),counter}),
    Req:ok([],[]);

handle('GET', ["update",StatName,NewValue],Req) ->
    erlstats:update_stat(name_to_atom(StatName),NewValue),
    Req:ok([],[]);

handle('GET', ["reset",StatName],Req) ->
    erlstats:reset_stat(name_to_atom(StatName)),
    Req:ok([],[]);

handle('GET', ["destroy",StatName],Req) ->
    erlstats:destroy_stat(name_to_atom(StatName)),
    Req:ok([],[]);

handle(_,_,Req) ->
    Req:ok([{"Content-Type", "text/plain"}], "Page not found\nErlstats Web").

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
convert_stat_value({Name, Value}) when is_atom(Value) ->
    {atom_to_binary(Name,latin1), atom_to_binary(Value, latin1)};
convert_stat_value({Name, Value}) when is_list(Value) ->
    {atom_to_binary(Name,latin1), list_to_binary(Value)};
convert_stat_value({Name, {H,M,S,MS}}) ->
    {atom_to_binary(Name,latin1), list_to_binary(integer_to_list(H)++":"++integer_to_list(M)++":"++integer_to_list(S)++"-"++integer_to_list(MS))};
convert_stat_value({Name, Value}) ->
    {atom_to_binary(Name,latin1), Value}.

name_to_atom(Name) when is_list(Name) ->
    list_to_atom(Name);
name_to_atom(Name) when is_binary(Name) ->
    atom_to_binary(Name,latin1);
name_to_atom(Name) when is_atom(Name) ->
    Name.
