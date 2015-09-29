-module(darwin_hello_handler).

-export([coap_discover/2, coap_get/4]).

-include_lib("gen_coap/include/coap.hrl").

coap_discover(Prefix, _Args) ->
    [{absolute, Prefix, []}].

coap_get(_ChId, _Prefix, [], _Request) ->
    {ok, #coap_resource{format = <<"text/plain">>, content = <<"hello">>}}.