-module(privatepaste_home).

-behavior(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
  io:format("~nLocale: ~p~n", [ privatepaste_http:language(Req)]),
    {ok, Body} = editor_dtl:render([],
                                 [{translation_fun, fun(Key, Locale) -> gettext:key2str(Key, "en") end},
                                  {locale, privatepaste_http:language(Req)}]),
    Headers = [{<<"Content-Type">>, <<"text/html">>}],
    {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

translate(Val, Locale) ->
  gettext:key2str(Val, Locale).
