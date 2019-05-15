-module(intf).
-author("Achil").

%% API
-export([new/0, add/4, remove/2, lookup/2, ref/2, name/2, list/1, broadcast/2]).
% intf module implements a map between name and reference, pid

% It should be quite straight forward to understand this.
% ref refers to a reference returned by monitor

new() -> [].

add(Name, Ref, Pid, Intf) -> [{Name, Ref, Pid} | Intf].

remove(_, []) -> [];
remove(Selected, [{Selected,_,_}|T]) -> T;
remove(Selected, [{Name,Ref,Pid}|T]) -> [{Name,Ref,Pid}|remove(Selected, T)].

lookup(_, []) -> notfound;
lookup(Selected, [{Selected,_,Pid}|_]) -> {ok, Pid};
lookup(Selected, [_|T]) -> lookup(Selected, T).

ref(_, []) -> notfound;
ref(Selected, [{Selected,Ref,_}|_]) -> {ok, Ref};
ref(Selected, [_|T]) -> ref(Selected, T).

name(_, []) -> notfound;
name(Ref, [{Name,Ref,_}|_]) -> {ok, Name};
name(Ref, [_|T]) -> name(Ref, T).

list(Intf) -> list(Intf,[]).
list([], L) -> L;
list([{Name,_,_}|T], L) -> list(T, [Name|L]).

broadcast(_, []) -> ok;
broadcast(Message, [{_,_,Pid}|T]) -> Pid ! Message, broadcast(Message, T).