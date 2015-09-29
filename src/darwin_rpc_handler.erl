-module(darwin_rpc_handler).

-export([coap_get/4]).

-include_lib("gen_coap/include/coap.hrl").

%% RPC Proxy
%% Fake at the moment, can't actually do posts or bodies
coap_get(_ChId, _Prefix, [],
         %#coap_message{payload=ReqPayload}) ->
         #coap_message{}) ->
    ReqPayload = "{\"auth\":{\"cik\":\"a32c85ba9dda45823be416246cf8b433baa068d7\"},\"calls\":[{\"id\":56,\"procedure\":\"read\",\"arguments\":[{\"alias\":\"command\"},{}]}]}",
    {ok,{{_Vsn,_StatusCode,_StatusText}, _Headers, Body}} =
        httpc:request(post, {"http://m2.exosite.com/onep:v1/rpc/process",
                            [{"Accept", "application/json; charset=utf-8"}],
                            "application/json; charset=utf-8",
                            ReqPayload}
                      , [], []),

    {ok, #coap_resource{format = <<"application/json">>, content = erlang:iolist_to_binary(Body)}}.
