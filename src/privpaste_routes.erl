%% --------------------------------------------------------------------
%% Abstracted Cowboy listener routes for ease of editing and management
%% --------------------------------------------------------------------
-module(privpaste_routes).

-export([get/0]).

-define(HEX, "abcdef0123456789").


is_paste_id(Id) ->
    Check = lists:filter(fun(C) -> lists:member(C, ?HEX) end, binary_to_list(Id)),
    length(Check) == 8.


%% @spec get() -> list
%% @doc Return the list of routes by host for the Cowboy listener
%% @end
%%
get() ->
    cowboy_router:compile([
            {'_',
                [
                    {
                        "/",
                        privpaste_handler_editor,
                        []
                    },
                    {
                        "/info",
                        privpaste_handler_info,
                        []
                    },
                    {
                        "/paste/[:paste_id]",
                        [{paste_id, fun(Id) -> is_paste_id(Id) end}],
                        privpaste_handler_paste,
                        []
                    },
                    {
                        "/edit/[:paste_id]",
                        [{paste_id, fun(Id) -> is_paste_id(Id) end}],
                        privpaste_handler_editor,
                        []
                    },
                    {
                        "/[:paste_id]",
                         [{paste_id, fun(Id) -> is_paste_id(Id) end}],
                         privpaste_handler_paste,
                         []
                    },
                    {
                        "/static/[...]",
                        cowboy_static,
                        {dir, [<<"static">>], [{mimetypes, cow_mimetypes, all},
                                               {dir_handler, directory_handler}]}
                   }
                ]
            }
        ]).
