-module(edflow_pt).
-export([parse_transform/2]).

parse_transform(GivenAST, _) ->
    modhack:patch(io, edf_io_overlay), 

    File = find_file(GivenAST),
    {ok, VanillaAST} = epp:parse_file(File, [], []),

    FilteredAST = lists:filter(fun not_parse_transform/1, VanillaAST),
    AST_send_fixed = parse_trans:plain_transform(fun replace_send/1, FilteredAST),
    AST_arity_fixed = parse_trans:plain_transform(fun fix_hm_arity/1, AST_send_fixed),

    AST = add_stuff(AST_arity_fixed),

    io:format("AST: ~p~n", [AST]),
    AST.

find_file([{attribute,_,file,{FileName,_}}|_]) ->
    put(filename, FileName),
    FileName;
find_file([_|Tail]) ->
    find_file(Tail);
find_file([]) ->
    undefined.

%% Filter function. It is used to remove parse_transform lines
%% from AST as they are removed when compiler calls parse_transform
not_parse_transform({attribute,_,compile,{parse_transform,edflow}}) ->
    false;
not_parse_transform(_) ->
    true.

%% Search for "-module" attribute and replace it with parametrized
%% version of it and define connectivity functions
add_stuff([{attribute,MLn,module,ModName}|AST]) ->
    [
        {attribute,MLn,module,{ModName,['Route']}}, 
        {attribute,0,export,[{handle_message,3}]} | 
        api_definitions() 
    ] ++ AST;
add_stuff([Term|AST]) ->
    [Term | add_stuff(AST)].

%% Return list of additional functions that proxy request to manager
api_definitions() ->
    [def_set_link(), def_get_link(), def_del_link()].

% set_link(Slot, Link) -> edflow:set_link(Route, Slot, Link).
def_set_link() ->
    {function,0,set_link,2,
        [{clause,0, [{var,0,'Slot'},{var,0,'Link'}], [],
                [{call,0,
                        {remote,0,{atom,0,edflow},{atom,0,set_link}},
                        [{var,0,'Route'},{var,0,'Slot'},{var,0,'Link'}]}]}]}.

% get_link(Slot) -> edflow:get_link(Route, Slot).
def_get_link() ->
    {function,0,get_link,1,
        [{clause,0, [{var,0,'Slot'}], [],
                [{call,0,
                        {remote,0,{atom,0,edflow},{atom,0,get_link}},
                        [{var,0,'Route'},{var,0,'Slot'}]}]}]}.

% del_link(Slot) -> edflow:del_link(Route, Slot).
def_del_link() ->
    {function,0,del_link,1,
        [{clause,0, [{var,0,'Slot'}], [],
                [{call,0,
                        {remote,0,{atom,0,edflow},{atom,0,del_link}},
                        [{var,0,'Route'},{var,0,'Slot'}]}]}]}.

%% Treat ordinary Erlang "pid ! Message" expressions as "Slot ! Message"
%% and replace them with "edflow:send_via(Route, Slot, Message)"
replace_send({'op', L, '!', Lhs, Rhs}) ->
    [NewLhs] = parse_trans:plain_transform(fun replace_send/1, [Lhs]),
    [NewRhs] = parse_trans:plain_transform(fun replace_send/1, [Rhs]),
    {call, L, {remote, L, {atom, L, edflow}, {atom, L, send_via}},
        [{var,L,'Route'}, NewLhs, NewRhs]};
replace_send(_) ->
    continue.

%% Fix handle_message arity:
%% handle_message(Slot, Message)  ~>  handle_message(Slot, Message, _State)
fix_hm_arity({function,Lf,handle_message,2,Clauses}) ->
%[{clause,4, [{atom,4,slot1},{var,4,'X'},{var,4,'State'}],
    FixedClauses = lists:map(fun fix_hm_clause/1, Clauses),
    {function,Lf,handle_message,3,FixedClauses};
fix_hm_arity(_) ->
    continue.

fix_hm_clause({clause, L, [Slot, Message], Guards, Body}) ->
    {clause, L, [Slot, Message, {var, L, '_State'}], Guards, Body}.
