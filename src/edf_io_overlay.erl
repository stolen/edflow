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
    {Destination, EvenMoreTokens} = read_till_pipe_or_arrow(MoreTokens),
    [{atom,Ls,handle_message}, {'(',Ls},
        Slot, {',',Lq}] ++ Destination ++ [{')',Lq} | filter_tokens(EvenMoreTokens)];

% Replace "(slot !:)" with "del_link(slot)"
filter_tokens([LPar={'(',Llp}, Slot = {atom, _, _}, {'!', _}, {':', _}, RPar={')', _} | MoreTokens]) ->
    [{atom,Llp,del_link}, LPar, Slot, RPar | filter_tokens(MoreTokens)];
% Replace "(slot !:" with "set_link(slot,"
filter_tokens([LPar={'(',Llp}, Slot = {atom, _, _}, {'!', _}, {':', Lc} | MoreTokens]) ->
    [{atom,Llp,set_link}, LPar, Slot, {',', Lc} | filter_tokens(MoreTokens)];

% Replace "#? slot" with "get_link(slot)"
filter_tokens([{'#', Lsh}, {'?', Lq}, Slot = {atom, Ls, _} | MoreTokens]) ->
    [{atom,Lsh,get_link}, {'(', Lq}, Slot, {')', Ls} | filter_tokens(MoreTokens)];

filter_tokens([Token | MoreTokens]) ->
    [Token | filter_tokens(MoreTokens)];
filter_tokens([]) -> [].

read_till_pipe_or_arrow(Tokens) ->
    read_till_pipe_or_arrow(Tokens, []).

read_till_pipe_or_arrow(Tokens = [{'->',_}|_], RevResult) ->
    {lists:reverse(RevResult), Tokens};
% Treat pipe as 'when' keyword
read_till_pipe_or_arrow([{'|',L} | Tokens], RevResult) ->
    {lists:reverse(RevResult), [{atom, L, 'when'} | Tokens]};

read_till_pipe_or_arrow([Token|MoreTokens], RevResult) ->
    read_till_pipe_or_arrow(MoreTokens, [Token|RevResult]).
