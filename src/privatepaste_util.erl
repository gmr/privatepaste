-module(privatepaste_util).

-export([get_int_value/2,
         get_language/1]).

get_int_value(OS, App) ->
    case os:getenv(OS) of
        false ->
            {ok, Port} = application:get_env(App),
            Port;
        Other ->
            list_to_integer(Other)
    end.

get_language(Req) ->
    case cowboy_req:parse_header(<<"accept-language">>, Req) of
        undefined -> list_to_binary(gettext:default_lang());
        Accept ->
            Ls = [binary_to_list(L) || {L, _} <- Accept],
            All = gettext:all_lcs(),
            case [L || L <- Ls, lists:member(L, All)] of
                [] -> list_to_binary(gettext:default_lang());
                [Lang|_] -> Lang
            end
    end.
