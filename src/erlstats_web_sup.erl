
-module(erlstats_web_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    error_logger:info_msg("Getting port from config~n"),
    {ok, Port} = application:get_env(port),
    Server = {erlstats_web_server, {erlstats_web_server, start_link,[Port]},
		   permanent, 5000, worker, [erlstats_web_server]},
    {ok, { {one_for_one, 5, 10}, [Server]}}.

