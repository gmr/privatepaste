%% --------------------------------------------------------------------
%% Abstracted Cowboy listener routes for ease of editing and management
%% --------------------------------------------------------------------
-module(privatepaste_routes).

-export([get/0]).

%% @spec get() -> list
%% @doc Return the list of routes by host for the Cowboy listener
%% @end
%%
get() ->
    [
        {'_',
            [
                {"/",     privatepaste_home, []},
                {"/info", privatepaste_info, []},
                {"/static/[...]", cowboy_static,
                 {dir, [<<"static">>], [{mimetypes, cow_mimetypes, all},
                                        {dir_handler, directory_handler}]}
                }
            ]
        }
    ].
