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
    %% Required Applications
    inets:start(),
    ssl:start(),

    application:start(gen_coap),
    coap_server_content:add_handler([<<"hello">>], darwin_hello_handler, undefined),
    coap_server_content:add_handler([<<"ts">>], darwin_timestamp_handler, undefined),
    coap_server_content:add_handler([<<"a1">>], darwin_dataport_handler, undefined),
    coap_server_content:add_handler([<<"rpc">>], darwin_rpc_handler, undefined),
    coap_server_content:add_handler([<<"provision">>, <<"activate">>], darwin_provision_activate_handler, undefined),
    coap_server_content:add_handler([<<"provision">>, <<"download">>], darwin_provision_content_handler, undefined),
    {ok, {{one_for_one, 3, 10}, []}}.