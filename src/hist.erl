-module(hist).
-author("Achil").

%% API
-export([new/1, update/3]).

% new: creates a new history. Messages from given name should always be ignored
new(Name) -> [{Name, inf}].


%update: updates history in case of more recent message
update(Node, N, History) ->
  Check = check(Node, N, History),
  case Check of
    new -> {new, replace(Node, N, History)};
    old -> old
  end
.

% check: checks whether history should be updated
check(_, _, []) -> new;
check(Node, N, History) ->
  [H | T] = History,
  case H of
    {Node, C} ->
      case N > C of
        true -> new;
        false -> old
      end;
    _ -> check(Node, N, T)
  end.


% replaces an entry with a new one
replace(Node, N, []) -> [{Node, N}];
replace(Node, N, [{Node, _} | T]) -> [{Node, N} | T];
replace(Node, N, [H | T]) -> [H | replace(Node, N, T)].