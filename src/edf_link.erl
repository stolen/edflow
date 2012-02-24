-module(edf_link).

% Create link process and (optionally) link it to caller
-export([create/0, create_link/0]).
-export([create/2, create_link/2]).
-export([add/2, add/3, send/3]).

create_link() -> create_link([], []).
create()      -> create([], []).

create_link(Left, Right) ->
    {ok, Pid} = create(Left, Right),
    erlang:link(Pid),
    {ok, Pid}.

create(Left, Right) ->
    Pid = spawn(fun() -> loop(Left, Right) end),
    {ok, Pid}.

loop(Left, Right) ->
    receive
        {add, left, Pid} ->
            loop([Pid|Left], Right);
        {add, right, Pid} ->
            loop(Left, [Pid|Right]);

        % TODO: maybe filter out sender
        {send, left, Message, _Sender} ->
            [Pid ! Message || Pid <- Left],
            loop(Left, Right);
        {send, right, Message, _Sender} ->
            [Pid ! Message || Pid <- Right],
            loop(Left, Right);
        {send, all, Message, _Sender} ->
            [Pid ! Message || Pid <- Left],
            [Pid ! Message || Pid <- Right],
            loop(Left, Right);

        _ ->
            % TODO: handle 'EXIT' messages
            loop(Left, Right)
    end.

% API
send(Link, Direction, Message) ->
    Link ! {send, Direction, Message, self()}.

add(Link, Direction) ->
    add(Link, Direction, self()).
add(Link, Direction, Pid) ->
    Link ! {add, Direction, Pid}.
