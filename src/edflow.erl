-module(edflow).

-export([parse_transform/2]).
-export([get_link/2, set_link/3, del_link/2, send_via/3]).

% Redirect parse_transform
parse_transform(AST, Options) ->
    edflow_pt:parse_transform(AST, Options).

% edflow API
get_link(Route, Slot) ->
    io:format("Getting ~w.~w~n", [Route, Slot]),
    {link, Slot}.

set_link(Route, Slot, Link) ->
    io:format("Setting ~w.~w = ~w~n", [Route, Slot, Link]),
    {set, Slot, Link}.

del_link(Route, Slot) ->
    io:format("Deleting ~w.~w~n", [Route, Slot]),
    {deleted, Slot}.

send_via(Route, Slot, Message) ->
    io:format("Sending ~w via ~w.~w~n", [Message, Route, Slot]),
    Message.
