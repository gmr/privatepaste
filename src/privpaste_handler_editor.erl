-module(privpaste_edit).

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
    Opts = [{translation_fun, ?TRANSLATE,
            {locale, privpaste_util:get_language(Req)}],
    {ok, Body} = editor_dtl:render([{modes, ?MODES}], Opts),
    {Body, Req, State}.
