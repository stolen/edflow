-module(edf_slot_db).
-export([init/0, connect/4, disconnect/2, get/2]).

init() ->
    dict:new().

connect(SlotID, LinkID, OutDirection, DB) ->
    % TODO: check if slot is already assigned to a link
    dict:store(SlotID, {LinkID, OutDirection}, DB).

disconnect(SlotID, DB) ->
    dict:erase(SlotID, DB).

get(SlotID, DB) ->
    case dict:find(SlotID, DB) of
        {ok, LinkID} ->
            LinkID;
        _ ->
            undefined
    end.
