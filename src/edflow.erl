-module(edflow).
-export([parse_transform/2]).

parse_transform(GivenAST, _) ->
    modhack:patch(io, edf_io_overlay), 

    File = find_file(GivenAST),
    {ok, VanillaAST} = epp:parse_file(File, [], []),

    FilteredAST = lists:filter(fun not_parse_transform/1, VanillaAST),
    AST = insert_exports(FilteredAST),
    io:format("AST: ~p~n", [AST]),
    AST.

find_file([{attribute,_,file,{FileName,_}}|_]) ->
    put(filename, FileName),
    FileName;
find_file([_|Tail]) ->
    find_file(Tail);
find_file([]) ->
    undefined.

not_parse_transform({attribute,_,compile,{parse_transform,edflow}}) ->
    false;
not_parse_transform(_) ->
    true.

insert_exports([ModDef = {attribute,_,module,_}|AST]) ->
    [ModDef, {attribute,0,export,[{handle_message,2}]} | AST];
insert_exports([Term|AST]) ->
    [Term | insert_exports(AST)].

