-module(privatepaste_home).

-behavior(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Type, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
  Opts = [{translation_fun, fun(Key, Locale) -> gettext:key2str(Key, Locale) end},
          {locale, privatepaste_http:language(Req)}],
  {ok, Body} = editor_dtl:render([], Opts),
  Headers = [{<<"Content-Type">>, <<"text/html">>}],
  {ok, Req2} = cowboy_req:reply(200, Headers, Body, Req),
  {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
