-module(darwin_dataport_handler).

-export([coap_discover/2, coap_get/4]).

-include_lib("gen_coap/include/coap.hrl").

coap_discover(Prefix, _Args) ->
    [{absolute, Prefix, []}].

%% Single Alias Read
coap_get(_ChId, _Prefix, [Alias], #coap_message{options=[{uri_query, [CIK]} | _T]}) ->
    {ok,{{_Vsn,_StatusCode,_StatusText}, _Headers, Body}} =
        httpc:request(get, {"http://m2.exosite.com/onep:v1/stack/alias?" ++ binary_to_list(Alias),
                            [{"X-Exosite-CIK", binary_to_list(CIK)},
                             {"Accept", "application/x-www-form-urlencoded; charset=utf-8"}]}
                      , [], []),

    [{_DPAlias, DPValue}] = www_form_decode(Body),

    {ok, #coap_resource{format = <<"text/plain">>, content = erlang:iolist_to_binary(DPValue)}}.

%% Helper Functions
www_form_decode(Str) ->
    NoPlusStr = binary_to_list(list_to_binary(re:replace(Str, "[\+]", " "))),
    UrlEnocdedList = [string:tokens(X, "=") || X <- string:tokens(NoPlusStr, "&")],
    [{http_uri:decode(Y),http_uri:decode(Z)} || [Y,Z] <- UrlEnocdedList].