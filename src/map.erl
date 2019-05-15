-module(map).
-author("Achil").

%% API
-export([all_nodes/1, reachable/2, update/3, new/0]).


% returns an empty map.
new() ->
  [].


% update: creates/updates an entry of the map
%%
% if map is empty, the entry is added and the result is returned
update(Key, NewValue, []) -> [{Key, NewValue}];
% if key matches the key of the head, update the entry (head of list) and return the map
update(Key, NewValue, [{Key, _} | T]) -> [{Key, NewValue} | T];
% else, recall the function inside the tail
update(Key, NewValue, [{DifferentKey, V} | T]) -> [{DifferentKey, V} | update(Key, NewValue, T)].


% reachable: returns the value that corresponds to a given key
%%
% if map is empty, no match was found
reachable(_, []) -> [];
% if the key matches the key of the head, return the value
reachable(Key, [{Key, Value} | _]) -> Value;
% else, recall function for tail
reachable(Key, [_ | T]) -> reachable(Key, T).


% all_nodes: returns all elements, both keys and values
all_nodes(Map) ->
  % flatten each entry of map, results in a list of lists
  M = lists:map(fun({E, L}) -> [E | L] end, Map),
  % flatten the given list
  F = lists:flatten(M),
  % remove duplicates by sorting
  lists:usort(F).
