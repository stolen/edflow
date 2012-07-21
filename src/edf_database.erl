%%% edf_database
%%% Abstraction above ETS (currently) to perform routing

%%% Instance table format:
%%%     {component(), pid()}

%%% Link table format:
%%%     {component(), slot(), slot(), component()

%%% Cache table format:
%%%     {pid(), slot(), slot(), component(), pid()}

-module(edf_database).
-author({"Danil Zagoskin", z@gosk.in}).

-export([start_link/0, stop/0]).
-export([register/2, get_pid/1, slot_routes/1, add_link/2]).

-export([init/1, handle_call/3, terminate/2]).

-type component() :: term().

-type instance() :: component() | pid().

-type slotname() :: atom().
-type subslot() :: integer().

-type slot() :: {slotname(), subslot()}.

-type endpoint() :: {instance(), slot()}.

-type linkpoint() :: {instance(), slotname()}.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
  gen_server:call(?MODULE, stop).

-record(db, {
  }).

init([]) ->
  ensure_table_exists(edf_instances, set),
  ensure_table_exists(edf_links, bag),
  ensure_table_exists(edf_subslots, set),
  {ok, #db{}}.

ensure_table_exists(Table, Type) ->
  case ets:info(Table, name) of
    undefined ->
      ets:new(Table, [Type, public, named_table, {write_concurrency,true}, {read_concurrency, true}]);
    _ ->
      ok
  end.

handle_call(stop, _, DB) ->
  {stop, shutdown, ok, DB};

handle_call(_, _, DB) ->
  {reply, {error, not_implemented}, DB}.

terminate(_Reason, _DB) ->
  ets:delete(edf_instances),
  ets:delete(edf_links),
  ets:delete(edf_subslots),
  ok.

-spec get_next_subslot({component(), slotname()}) -> subslot().

get_next_subslot(_Endpoint = {Pid, SlotName}) when is_pid(Pid) ->
  get_next_subslot({get_component(Pid), SlotName});

get_next_subslot(Endpoint = {_Component, SlotName}) when is_atom(SlotName) ->
  NextSS = case ets:lookup(edf_subslots, Endpoint) of
    [] ->
      0;
    [{Endpoint, PrevSS}] ->
      PrevSS + 1
  end,
  ets:insert(edf_subslots, {Endpoint, NextSS}),
  NextSS.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec get_pid(component()) -> pid().
get_pid(Component) ->
  case ets:lookup(edf_instances, Component) of
    [] ->
      {error, notfound};
    [{Component, Pid}] ->
      {ok, Pid};
    [_|_] ->
      {error, duplicate}
  end.

-spec get_component(pid()) -> component().
get_component(Pid) ->
  erlang:error({not_implemented, get_component}).


-spec register(pid() | component(), component() | pid()) -> ok.
register(Pid, Component) when is_pid(Pid) ->
  ?MODULE:register(Component, Pid);
register(Component, Pid) when is_pid(Pid) ->
  true = ets:insert(edf_instances, {Component, Pid}),
  % TODO: Monitor pid for later removal
  ok.

-spec slot_routes(slotname() | slot()) -> [endpoint()].
slot_routes(SlotName) ->
  not_implemented.

-spec add_link(linkpoint(), linkpoint()) -> {ok, {subslot(), subslot()}}.
add_link(LP1 = {Component1, SlotName1}, LP2 = {Component2, SlotName2}) ->
  SS1 = get_next_subslot(LP1),
  SS2 = get_next_subslot(LP2),
  Slot1 = {SlotName1, SS1},
  Slot2 = {SlotName2, SS2},
  ets:insert(edf_links, {Component1, Slot1, Slot2, Component2}),
  {ok, {SS1, SS2}}.
