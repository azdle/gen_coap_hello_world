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
    coap_server:start(),
    {ok, {{one_for_one, 3, 10},
        [
         {hello_handler, {darwin_hello_handler, start_link, []},
            permanent, 10000, worker, [darwin_hello_handler]}
        ]}}.