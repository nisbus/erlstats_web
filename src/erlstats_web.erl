-module(erlstats_web).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    error_logger:info_msg("Starting erlstats_web~n"),
    Dispatch = [
		{'_', [{'_', cowboy_http, []}]}
	       ],
    {ok,Port} = application:get_env(port),
    io:format("got port from config ~p~n",[Port]),
    cowboy:start_listener(http, Port, 
			  cowboy_tcp_transport, [{port, Port}],
			  cowboy_http_protocol, [{dispatch,Dispatch}]
			 ),
    io:format("cowboy started~n"),

    erlstats_web_sup:start_link().

stop(_State) ->
    ok.
