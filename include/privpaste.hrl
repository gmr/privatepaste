%% ------------------------------------------------------------------
%% Common records and macros
%% ------------------------------------------------------------------

-record(state, {languages}).

-record(paste, {id :: string(),
                hostname :: string(),
                owner :: string(),
                created_at :: string(),
                updated_at :: string(),
                revision :: integer(),
                ttl :: integer(),
                password :: string(),
                views :: integer(),
                syntax :: string(),
                line_numbers :: atom(),
                remote_ip :: string(),
                request_headers :: list(),
                mime_type :: string(),
                content :: string()}).

-define(DEFAULT_LANGUAGE, "en").

-define(EMPTY, <<"">>).
-define(ERROR, <<"error">>).

-define(CONTENT_LENGTH, <<"content-length">>).
-define(CONTENT_TYPE,   <<"content-type">>).
-define(MIME_TYPE_HTML, <<"text/html">>).
-define(MIME_TYPE_JSON, <<"application/json">>).

-define(CONTENT_TYPE_HTML, {?CONTENT_TYPE, ?MIME_TYPE_HTML}).
-define(CONTENT_TYPE_JSON, {?CONTENT_TYPE, ?MIME_TYPE_JSON}).

-define(STATUS_CODES, [{400, <<"Bad Request">>},
                       {401, <<"Unauthorized">>},
                       {402, <<"Payment Required">>},
                       {403, <<"Forbidden">>},
                       {404, <<"Not Found">>},
                       {405, <<"Method Not Allowed">>},
                       {406, <<"Not Acceptable">>},
                       {407, <<"Proxy Authentication Required">>},
                       {408, <<"Request Timeout">>},
                       {409, <<"Conflict">>},
                       {410, <<"Gone">>},
                       {411, <<"Length Required">>},
                       {412, <<"Precondition Failed">>},
                       {413, <<"Request Entity Too Large">>},
                       {414, <<"Request-URI Too Long">>},
                       {415, <<"Requested Range Not Satisfiable">>},
                       {417, <<"Expectation Failed">>},
                       {500, <<"Internal Server Error">>},
                       {501, <<"Not Implemented">>},
                       {502, <<"Bad Gateway">>},
                       {503, <<"Service Unavailable">>},
                       {504, <<"Gateway Timeout">>},
                       {505, <<"HTTP Version Not Supported">>}]).

-define(MODES, [{<<"none">>, <<"None">>},
                {<<"apl">>, <<"apl">>},
                {<<"asterisk">>, <<"Asterisk">>},
                {<<"c">>, <<"C">>},
                {<<"c++">>, <<"C++">>},
                {<<"c#">>, <<"C#">>},
                {<<"clojure">>, <<"Clojure">>},
                {<<"cobol">>, <<"Cobol">>},
                {<<"coffeescript">>, <<"Coffeescript">>},
                {<<"commonlisp">>, <<"Lisp">>},
                {<<"css">>, <<"CSS">>},
                {<<"erlang">>, <<"Erlang">>},
                {<<"go">>, <<"Go">>},
                {<<"groovy">>, <<"Groovy">>},
                {<<"haml">>, <<"Haml">>},
                {<<"haskell">>, <<"Haskell">>},
                {<<"htmlmixed">>, <<"HTML">>},
                {<<"http">>, <<"HTTP">>},
                {<<"jade">>, <<"Jade">>},
                {<<"java">>, <<"Java">>},
                {<<"javascript">>, <<"Javascript">>},
                {<<"json">>, <<"JSON">>},
                {<<"jinja2">>, <<"Jinja2">>},
                {<<"less">>, <<"LESS">>},
                {<<"lua">>, <<"Lua">>},
                {<<"markdown">>, <<"Markdown">>},
                {<<"mirc">>, <<"IRC">>},
                {<<"nginx">>, <<"Nginx">>},
                {<<"ocaml">>, <<"OCaml">>},
                {<<"pascal">>, <<"Pascal">>},
                {<<"perl">>, <<"Perl">>},
                {<<"php">>, <<"PHP">>},
                {<<"pig">>, <<"Pig">>},
                {<<"properties">>, <<"Properties">>},
                {<<"python">>, <<"Python">>},
                {<<"q">>, <<"Q">>},
                {<<"r">>, <<"R">>},
                {<<"rpm">>, <<"RPM">>},
                {<<"rst">>, <<"ReStructuredText">>},
                {<<"ruby">>, <<"Ruby">>},
                {<<"rust">>, <<"Rust">>},
                {<<"sass">>, <<"SASS">>},
                {<<"scala">>, <<"Scala">>},
                {<<"scheme">>, <<"Scheme">>},
                {<<"shell">>, <<"Shell">>},
                {<<"sieve">>, <<"Sieve">>},
                {<<"smalltalk">>, <<"Smalltalk">>},
                {<<"smarty">>, <<"Smarty">>},
                {<<"sparql">>, <<"Sparql">>},
                {<<"sql">>, <<"SQL">>},
                {<<"tkl">>, <<"Tkl">>},
                {<<"tiddlywiki">>, <<"TiddlyWiki">>},
                {<<"tiki">>, <<"Tiki">>},
                {<<"turtle">>, <<"Turtle">>},
                {<<"vb">>, <<"VB">>},
                {<<"vbscript">>, <<"VBScript">>},
                {<<"velocity">>, <<"Velocity">>},
                {<<"verilog">>, <<"Verilog">>},
                {<<"xml">>, <<"XML">>},
                {<<"xquery">>, <<"XQuery">>},
                {<<"yaml">>, <<"YAML">>},
                {<<"z80">>, <<"z80">>}]).

-define(TTLS, [{360, <<"5 Minutes">>},
               {900, <<"15 Minutes">>},
               {1800, <<"30 Minutes">>},
               {3600, <<"1 Hour">>},
               {86400, <<"1 Day">>},
               {432000, <<"5 Days">>},
               {604800, <<"1 Week">>},
               {2592000, <<"1 Month">>},
               {7776000, <<"3 Months">>},
               {15552000, <<"6 Months">>},
               {31536000, <<"1 Year">>}]).
-define(TTL_DEFAULT, 432000).
-define(TTL_MAX, 31536000).
