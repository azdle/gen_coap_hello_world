-module(darwin_provision_content_handler).

-export([coap_get/4]).

-include_lib("gen_coap/include/coap.hrl").

%% List Content IDs
coap_get(_ChId, _Prefix, [Vendor, Model], Request) ->
    CIK = get_cik_from_request(Request),
    case get_content_ids(CIK, Vendor, Model) of
        {ok, ContentList} ->
            {ok, #coap_resource{format = <<"text/plain">>, content = erlang:iolist_to_binary(ContentList)}};
        {error, unauthorized} ->
            {ok, #coap_resource{format = <<"text/plain">>, content = <<"Unauthorized">>}};
        {error, forbidden} ->
            {ok, #coap_resource{format = <<"text/plain">>, content = <<"Forbidden">>}};
        {error, _} ->
            {ok, #coap_resource{format = <<"text/plain">>, content = <<"Internal Server Error">>}}
    end;


%% Get Content Info
coap_get(_ChId, _Prefix, [Vendor, Model, Id, <<"info">>], Request) ->
    CIK = get_cik_from_request(Request),
    case get_content_info(CIK, Vendor, Model, Id) of
        {ok, Content} ->
            {ok, #coap_resource{content = erlang:iolist_to_binary(Content)}};
        {error, unauthorized} ->
            {ok, #coap_resource{format = <<"text/plain">>, content = <<"Unauthorized">>}};
        {error, forbidden} ->
            {ok, #coap_resource{format = <<"text/plain">>, content = <<"Forbidden">>}};
        {error, _} ->
            {ok, #coap_resource{format = <<"text/plain">>, content = <<"Internal Server Error">>}}
    end;

%% Download Content
coap_get(_ChId, _Prefix, [Vendor, Model, Id], Request) ->
    CIK = get_cik_from_request(Request),
    case get_content(CIK, Vendor, Model, Id) of
        {ok, Content} ->
            {ok, #coap_resource{content = erlang:iolist_to_binary(Content)}};
        {error, unauthorized} ->
            {ok, #coap_resource{format = <<"text/plain">>, content = <<"Unauthorized">>}};
        {error, forbidden} ->
            {ok, #coap_resource{format = <<"text/plain">>, content = <<"Forbidden">>}};
        {error, _} ->
            {ok, #coap_resource{format = <<"text/plain">>, content = <<"Internal Server Error">>}}
    end.


%%
get_content_ids(CIK, Vendor, Model) when
        is_binary(CIK) ->
    {ok,{{_Vsn,StatusCode,_StatusText}, _Headers, Body}} =
        httpc:request(get, {"http://m2.exosite.com/provision/download?" ++
                            "vendor=" ++ binary_to_list(Vendor) ++
                            "&model=" ++ binary_to_list(Model),
                            [{"X-Exosite-CIK", binary_to_list(CIK)}]}
                      , [], []),

    case StatusCode of
        200 ->
            {ok, Body};
        401 ->
            {error, unauthorized};
        403 ->
            {error, forbidden};
        _ ->
            {error, internal}
    end.

get_content_info(CIK, Vendor, Model, Id) when
        is_binary(CIK) ->
    {ok,{{_Vsn,StatusCode,_StatusText}, _Headers, Body}} =
        httpc:request(get, {"http://m2.exosite.com/provision/download?" ++
                            "vendor=" ++ binary_to_list(Vendor) ++
                            "&model=" ++ binary_to_list(Model) ++
                            "&id=" ++ binary_to_list(Id) ++
                            "&info=true",
                            [{"X-Exosite-CIK", binary_to_list(CIK)}]}
                      , [], []),

    case StatusCode of
        200 ->
            {ok, Body};
        401 ->
            {error, unauthorized};
        403 ->
            {error, forbidden};
        X when X >= 500, X =< 599 ->
            {error, gateway};
        _ ->
            {error, internal}
    end.

get_content(CIK, Vendor, Model, Id) when
        is_binary(CIK) ->
    {ok,{{_Vsn,StatusCode,_StatusText}, _Headers, Body}} =
        httpc:request(get, {"http://m2.exosite.com/provision/download?" ++
                            "vendor=" ++ binary_to_list(Vendor) ++
                            "&model=" ++ binary_to_list(Model) ++
                            "&id=" ++ binary_to_list(Id),
                            [{"X-Exosite-CIK", binary_to_list(CIK)}]}
                      , [], []),

    case StatusCode of
        200 ->
            {ok, Body};
        401 ->
            {error, unauthorized};
        403 ->
            {error, forbidden};
        X when X >= 500, X =< 599 ->
            {error, gateway};
        _ ->
            {error, internal}
    end.


%% Helper Functions
www_form_decode(Str) ->
    NoPlusStr = binary_to_list(list_to_binary(re:replace(Str, "[\+]", " "))),
    UrlEnocdedList = [string:tokens(X, "=") || X <- string:tokens(NoPlusStr, "&")],
    [{http_uri:decode(Y),http_uri:decode(Z)} || [Y,Z] <- UrlEnocdedList].

get_cik_from_request(#coap_message{options=Options}) ->
    [CIK|_] = proplists:get_value(uri_query, Options),
    CIK.
