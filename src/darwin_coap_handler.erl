-module(darwin_coap_handler).
-behavior(gen_server).

-include_lib("gen_coap/include/coap.hrl").

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).


start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    coap_server:start(),
    coap_server_content:add_handler(self(),
        [{absolute, ["hello"], []},          % Hello
         {absolute, ["hello", who], []},     % Hello, <Name>
         {absolute, ["a1", alias], []}       % Alias Read/Write
        ]),
    {ok, []}.

%% callbacks
handle_call(_Msg, _From, State) ->
    {reply, unknown_command, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

% Hello, <Name>
handle_info({coap_request, _ChId, Channel, _Match, Request=#coap_message{method='get',options=[{uri_path, ["hello", Name]}]}}, State) ->
    coap_request:reply_content(Channel, Request, <<"text/plain">>, erlang:iolist_to_binary([<<"hello, ">>, Name])),
    {noreply, State};
% Hello
handle_info({coap_request, _ChId, Channel, _Match, Request=#coap_message{method='get',options=[{uri_path, ["hello"]}]}}, State) ->
    coap_request:reply_content(Channel, Request, <<"text/plain">>, <<"hello, world">>),
    {noreply, State};
% Alias Read
handle_info({coap_request, _ChId, Channel, _Match, Request=#coap_message{method='get',options=[{uri_path, ["a1", Name]}]}}, State) ->
    coap_request:reply_content(Channel, Request, <<"text/plain">>, erlang:iolist_to_binary([<<"hello, ">>, Name])),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.