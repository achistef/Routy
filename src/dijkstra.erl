-module(dijkstra).
-author("Achil").

%% API
-export([route/2, table/2]).


% entry: returns the length of the shortest path a node or 0 if the node is not found
%%
% node was not found, returns zero
entry(_, []) -> 0;
% if the node matches the node at the head, return its length
entry(Node, [{Node, L, _} | _]) -> L;
% else, recall the function for the tail
entry(Node, [_ | T]) -> entry(Node, T).


% containsEntry: checks whether a list contains an entry
%%
% if list is empty, return false
containsEntry(_, []) -> false;
% if node matches the node at the head of the list, return true
containsEntry(Node, [{Node, _, _} | _]) -> true;
% else, recall function for tail
containsEntry(Node, [_ | T]) -> containsEntry(Node, T).


% removeEntry: removes an entry from a list, based on the node given
% IMPORTANT: list MUST contain node
%%
% if the node matches the node at the head of the list, ignore the head and return the tail
removeEntry(Node, [{Node, _, _} | T]) -> T;
% else, keep the head and recall the function for the tail
removeEntry(Node, [{H, L, G} | T]) -> [{H, L, G} | removeEntry(Node, T)].


% insertEntry: inserts an element into the sorted list
% this functions respects the order of the list
%%
% if list is empty, return a list with the given entry
insertEntry(Node, N, Gateway, []) -> [{Node, N, Gateway}];
% else, insert the entry to sorted list
insertEntry(Node, N, Gateway, Sorted) ->
  % extract values from the head of sorted list
  [{Another, L, G} | T] = Sorted,
  % compare given length with head's length
  case N < L of
    % if given length is smaller, prepend given entry to list
    true -> [{Node, N, Gateway} | Sorted];
    %else, keep the head and recall the function for the tail of the list.
    false -> [{Another, L, G} | insertEntry(Node, N, Gateway, T)]
  end.


% replace: replaces the entry for Node in Sorted with a new entry having a new length N and Gateway.
replace(Node, N, Gateway, Sorted) ->
  %check if list contains node
  C = containsEntry(Node, Sorted),
  case C of
    true ->
      % remove previous entry for node
      R = removeEntry(Node, Sorted),
      % insert new entry for node
      insertEntry(Node, N, Gateway, R);
    false ->
      % do nothing
      Sorted
  end.


% update: updates the shortest path for a specific entry
update(Node, N, Gateway, Sorted) ->
  % find current length to given node
  E = entry(Node, Sorted),
  % update should happen if current length is greater than zero and bigger than given length
  ShouldUpdate = E > 0 andalso N < E,
  case ShouldUpdate of
    % return  updated list
    true -> replace(Node, N, Gateway, Sorted);
    % do nothing
    false -> Sorted
  end.


% insertGateways: updates the dummy entries for the gateways
%%
% if list is empty, all gateways were inserted
insertGateways([], Map) -> Map;
% recall the function for the tail after updating the map for the current gateway
insertGateways([Gateway | T], Map) -> insertGateways(T, update(Gateway, 0, Gateway, Map)).


% table: creates a routing table
table(Gateways, Map) ->
  % collect all nodes
  All = map:all_nodes(Map),
  % create a sorted list with dummy entries
  Dummy = lists:map(fun(Node) -> {Node, inf, unknown} end, All),
  % update the entries for the gateways
  Sorted = insertGateways(Gateways, Dummy),
  % start dijkstra algorithm
  iterate(Sorted, Map, []).


% route: returns the suited gateway for a given node
%%
% if table is empty, no gateway was found
route(_, []) -> notfound;
% else, search for gateway inside the table
route(Selected, Table) ->
  % extract values from the head of the table
  [{Node, Gateway}|T] = Table,
  case Node of
    % if selected node matches the node of the head, return the gateway
    Selected -> {ok, Gateway};
    % else, recall the function for the tail
    _ -> route(Selected, T)
  end.


% iterate: runs the dijkstra algorithm
%%
% if sorted list is empty, iteration is over
iterate([], _, Table) -> Table;
% if the length in the first entry is infinity, iteration is over
iterate([{_, inf, _}|_], _, Table) -> Table;
% else
iterate(Sorted, Map, Table) ->
  % extract values from the head of the list
  [{Node, L, Gateway} | Rest] = Sorted,
  % find all reachable nodes from the node of the head
  Reachables = map:reachable(Node, Map),
  % update sorted list with possibly new length for all reachable nodes
  UpdatedSorted = updateAll(Reachables, L + 1, Gateway, Rest),
  % link given gateway with node inside, because its the shortest path
  UpdatedTable = [{Node,Gateway}|Table],
  % recall the function for the rest of possible paths
  iterate(UpdatedSorted, Map, UpdatedTable).


% updateAll: updates nodes inside the sorted list
%%
% if there are no reachable nodes left return sorted
updateAll([], _, _, Sorted) -> Sorted;
% else
updateAll([Reachable | T], L, Gateway, Sorted) ->
  % update the entry for the head of the reachable list
  Updated = update(Reachable, L, Gateway, Sorted),
  % recall the function for the rest of the reachable nodes
  updateAll(T, L, Gateway, Updated).








































