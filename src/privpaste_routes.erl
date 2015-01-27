%% --------------------------------------------------------------------
%% Abstracted Cowboy listener routes for ease of editing and management
%% --------------------------------------------------------------------
-module(privpaste_routes).

-export([get/0]).

%% @spec get() -> list
%% @doc Return the list of routes by host for the Cowboy listener
%% @end
%%
get() ->
    [
        {'_',
            [
                {"/",                           privpaste_handler_editor, []},
                {"/edit/[:paste_id]",           privpaste_handler_editor, []},
                {"/edit/[:paste_id]/[:token]",  privpaste_handler_editor, []},
                {"/info",                       privpaste_handler_info, []},
                {"/[:paste_id]",                privpaste_handler_paste, []},
                {"/[:paste_id]/[:token]",       privpaste_handler_paste, []},
                {"/paste/[:paste_id]",          privpaste_handler_paste, []},
                {"/paste/[:paste_id]/[:token]", privpaste_handler_paste, []},
                {"/static/[...]",               cowboy_static, {dir, [<<"static">>], [{mimetypes, cow_mimetypes, all},
                                                                                      {dir_handler, directory_handler}]}}
            ]
        }
    ].
