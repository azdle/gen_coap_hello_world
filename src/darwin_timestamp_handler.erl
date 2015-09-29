-module(darwin_timestamp_handler).

-export([coap_discover/2, coap_get/4]).

-include_lib("gen_coap/include/coap.hrl").

coap_discover(Prefix, _Args) ->
    [{absolute, Prefix, []}].

coap_get(_ChId, _Prefix, [], _Request) ->
    {ok,{{_Vsn,200,"OK"}, _Headers, Body}} =
        httpc:request(get, {"http://m2.exosite.com/timestamp", []} , [], []),

    {ok, #coap_resource{format = <<"text/plain">>, content = erlang:iolist_to_binary(Body)}}.