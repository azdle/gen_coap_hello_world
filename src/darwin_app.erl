-module(darwin_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

-behavior(supervisor).
-export([init/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
	start(normal, []).

start(normal, _StartArgs) ->
    supervisor:start_link(?MODULE, _Arg = []).

stop(_State) ->
    ok.

init([]) ->
    {ok, {{one_for_one, 3, 10},[{coap_handler, {darwin_coap_handler, start_link, []},
                                 permanent, 10000, worker, [darwin_coap_handler]}]}}.