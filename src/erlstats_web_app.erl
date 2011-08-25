-module(erlstats_web_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    error_logger:info_msg("Starting erlstats_web~n"),
    erlstats_web_sup:start_link().

stop(_State) ->
    ok.
