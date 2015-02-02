%% ------------------------------------------------------------------
%% Misc Utility Functions
%% ------------------------------------------------------------------
-module(privpaste_util).

-export([erlydtl_opts/1,
         get_atom_from_proplist/2,
         get_atom_from_proplist/3,
         get_binary_from_proplist/2,
         get_binary_from_proplist/3,
         get_body_size/1,
         get_int_from_proplist/2,
         get_int_from_proplist/3,
         get_int_from_env/2,
         get_language/1]).

-include("privpaste.hrl").

erlydtl_opts(Req) ->
    Translate = fun(Key, Locale) ->
        gettext:key2str(Key, Locale)
    end,
    [{translation_fun, Translate, {locale, get_language(Req)}}].

get_atom_from_proplist(Key, Data) ->
    case proplists:get_value(list_to_binary(atom_to_list(Key)), Data) of
        null -> null;
        Value -> Value
    end.

get_atom_from_proplist(Key, Data, Default) ->
    case proplists:get_value(list_to_binary(atom_to_list(Key)), Data) of
        null -> Default;
        Value -> Value
    end.

get_binary_from_proplist(Key, Data) ->
    case proplists:get_value(list_to_binary(atom_to_list(Key)), Data) of
        null -> <<"">>;
        Value -> Value
    end.

get_binary_from_proplist(Key, Data, Default) ->
    case proplists:get_value(list_to_binary(atom_to_list(Key)), Data) of
        null  -> Default;
        Value -> Value
    end.

get_body_size(Body) when is_binary(Body) ->
    integer_to_list(byte_size(Body));

get_body_size(Body) when is_list(Body) ->
    integer_to_list(byte_size(list_to_binary(Body))).

get_int_from_env(OS, App) ->
    case os:getenv(OS) of
        false ->
            {ok, Port} = application:get_env(App),
            Port;
        Other ->
            list_to_integer(Other)
    end.

get_int_from_proplist(Key, Data) ->
    case proplists:get_value(list_to_binary(atom_to_list(Key)), Data) of
        null -> 0;
        Value -> get_int_value(Value)
    end.

get_int_from_proplist(Key, Data, Default) ->
    case proplists:get_value(list_to_binary(atom_to_list(Key)), Data) of
        null -> Default;
        Value -> get_int_value(Value)
    end.

get_int_value(Value) when is_binary(Value) ->
    binary_to_integer(Value);

get_int_value(Value) when is_list(Value) ->
    list_to_integer(Value);

get_int_value(Value) when is_integer(Value) ->
    Value.

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

