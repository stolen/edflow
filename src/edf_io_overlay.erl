-module(edf_io_overlay).
-export([scan_erl_form/3, '_module_backup'/1]).

-define(ORIG_IO, orig_io).
'_module_backup'(io) -> ?ORIG_IO.

scan_erl_form(Device, Prompt, StartLine) ->
    filter_result(?ORIG_IO:scan_erl_form(Device, Prompt, StartLine)).

filter_result({ok, Tokens, EndLine}) ->
    {ok, filter_tokens(Tokens), EndLine};
filter_result(Other) ->
    Other.

% Replace "slot ? Message" with handle_message(slot, Message)
filter_tokens([Slot = {atom, Ls, _}, {'?', Lq} | MoreTokens]) ->
    {Destination, EvenMoreTokens} = read_till_arrow(MoreTokens),
    [{atom,Ls,handle_message}, {'(',Ls},
        Slot, {',',Lq}] ++ Destination ++ [{')',Lq} | filter_tokens(EvenMoreTokens)];

filter_tokens([Token | MoreTokens]) ->
    [Token | filter_tokens(MoreTokens)];
filter_tokens([]) -> [].

read_till_arrow(Tokens) ->
    read_till_arrow(Tokens, []).

read_till_arrow(Tokens = [{'->',_}|_], RevResult) ->
    {lists:reverse(RevResult), Tokens};
read_till_arrow([Token|MoreTokens], RevResult) ->
    read_till_arrow(MoreTokens, [Token|RevResult]).
