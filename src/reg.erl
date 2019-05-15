-module(reg).
-author("Achil").

%% API
-export([start/1, connect/1, stop/1, broadcast/1, update/1]).

start([]) -> done;
start([City| Rest]) ->
  mrouty:start(City),
  start(Rest).

connect([]) -> done;
connect([Con| Rest]) ->
  {City, Links} = Con,
  link(City, Links),
  connect(Rest).

link(_, []) -> done;
link(City, Links) ->
  IP = '@192.168.1.3',
  [{Link, Region}| Rest] = Links,
  Destination  = list_to_atom(lists:concat([Region,IP])),
  City ! {add, Link, {Link, Destination}},
  link(City, Rest).

stop([]) -> done;
stop([City| Rest]) ->
  City ! stop,
  stop(Rest).

broadcast([]) -> done;
broadcast([City| Rest]) ->
  City ! broadcast,
  broadcast(Rest).

update([]) -> done;
update([City| Rest]) ->
  City ! update,
  update(Rest).