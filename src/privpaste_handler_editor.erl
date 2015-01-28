%% ------------------------------------------------------------------
%% Editor Page Endpoint
%% ------------------------------------------------------------------
-module(privpaste_handler_editor).

-export([init/2,
         content_types_provided/2,
         handle_html/2,
         terminate/3]).

-include("privpaste.hrl").

init(Req, Opts) ->
  {cowboy_rest, Req, Opts}.

content_types_provided(Req, State) ->
  {[
    {?MIME_TYPE_HTML, handle_html}
  ], Req, State}.

terminate(_Reason, _Req, _State) ->
    ok.

handle_html(Req, State) ->
    {ok, Body} = editor_dtl:render([{modes, ?MODES}, {syntax, <<"erlang">>}, {ttl, ?TTL_DEFAULT}, {ttls, ?TTLS}],
                                   privpaste_util:erlydtl_opts(Req)),
    {Body, Req, State}.
