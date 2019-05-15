-module(africa).
-author("Achil").

%% API
-export([connect/0, stop/0, start/0, update/0, broadcast/0]).

stop() ->
  Cities = [addisababa, diredawa, cairo, alexandria, lagos, kano],
  reg:stop(Cities).

start() ->
  Cities = [addisababa, diredawa, cairo, alexandria, lagos, kano],
  reg:start(Cities).

connect() ->
  Connections = [
    {addisababa, [{diredawa, africa}, {cairo, africa}]},
    {diredawa, [{addisababa, africa}]},
    {cairo, [{berlin, europe},{mumbai, asia}, {lagos, africa},{alexandria, africa},{addisababa, africa}]},
    {alexandria, [{cairo, africa}]},
    {lagos, [{kano, africa}, {cairo, africa}, {mexico, america}]},
    {kano, [{lagos, africa}]}
  ],
  reg:connect(Connections).

broadcast() ->
  Cities = [addisababa, diredawa, cairo, alexandria, lagos, kano],
  reg:broadcast(Cities).

update() ->
  Cities = [addisababa, diredawa, cairo, alexandria, lagos, kano],
  reg:update(Cities).