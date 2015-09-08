-module(darwin_dataport_handler).
-behavior(gen_server).

-include_lib("gen_coap/include/coap.hrl").

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).


start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    inets:start(),
    ssl:start(),
    coap_server_content:add_handler(self(),
        [{absolute, ["a1", alias], []}
        ]),
    {ok, []}.

%% callbacks
handle_call(_Msg, _From, State) ->
    {reply, unknown_command, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

% Alias Read
handle_info({coap_request, _ChId, Channel, _Match, Request=#coap_message{method='get',options=[{uri_query, [CIK]},{uri_path, ["a1", Alias]}]}}, State) ->
    
    {ok,{{_Vsn,_StatusCode,_StatusText}, _Headers, Body}} =
        httpc:request(get, {"http://m2.exosite.com/onep:v1/stack/alias?" ++ Alias,
                            [{"X-Exosite-CIK", CIK},
                             {"Accept", "application/x-www-form-urlencoded; charset=utf-8"}]}
                      , [], []),

    [{_DPAlias, DPValue}] = www_form_decode(Body),

    coap_request:reply_content(Channel, Request, <<"text/plain">>, erlang:iolist_to_binary(DPValue)),
    {noreply, State};
handle_info(_Info, State) ->
    io:format("No Match~n",[]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% Helper Functions
www_form_decode(Str) ->
    NoPlusStr = binary_to_list(list_to_binary(re:replace(Str, "[\+]", " "))),
    UrlEnocdedList = [string:tokens(X, "=") || X <- string:tokens(NoPlusStr, "&")],
    [{http_uri:decode(Y),http_uri:decode(Z)} || [Y,Z] <- UrlEnocdedList].