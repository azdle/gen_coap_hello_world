-module(darwin_provision_activate_handler).

-export([coap_get/4]).

-include_lib("gen_coap/include/coap.hrl").

%% Provision Activate
coap_get(_ChId, _Prefix, [Vendor, Model, Serial], _Request) ->
    {ok,{{_Vsn,StatusCode,_StatusText}, _Headers, Body}} =
        httpc:request(post, {"http://m2.exosite.com/provision/activate",
                            [],
                            "application/x-www-form-urlencoded; charset=utf-8",
                            "vendor=" ++ binary_to_list(Vendor) ++
                            "&model=" ++ binary_to_list(Model) ++
                            "&sn=" ++ binary_to_list(Serial)}
                      , [], []),

    case StatusCode of
        200 ->
            {ok, #coap_resource{format = <<"text/plain">>, content = erlang:iolist_to_binary(Body)}};
        404 ->
            {ok, #coap_resource{format = <<"text/plain">>, content = <<"Not Found">>}};
        409 ->
            {ok, #coap_resource{format = <<"text/plain">>, content = <<"Conflict">>}};
        X when X >= 500, X < 600 ->
            {ok, #coap_resource{format = <<"text/plain">>, content = <<"Gateway Error">>}};
        _ ->
            {ok, #coap_resource{format = <<"text/plain">>, content = <<"Internal Server Error">>}}
    end.
