-module(america).
-author("Achil").

%% API
-export([connect/0, stop/0, start/0, update/0, broadcast/0]).

stop() ->
  Cities = [montreal, toronto, nyc, losangeles, mexico, iztapalapa],
  reg:stop(Cities).

start() ->
  Cities = [montreal, toronto, nyc, losangeles, mexico, iztapalapa],
  reg:start(Cities).

connect() ->
  Connections = [
    {montreal, [{toronto, america}]},
    {toronto, [{montreal, america},{nyc, america}]},
    {nyc, [{losangeles, america},{mexico, america},{toronto, america},{berlin, europe}]},
    {losangeles, [{nyc, america}]},
    {mexico, [{iztapalapa, america}, {nyc, america}, {lagos, africa}]},
    {iztapalapa, [{mexico, america}]}
  ],
  reg:connect(Connections).

broadcast() ->
  Cities = [montreal, toronto, nyc, losangeles, mexico, iztapalapa],
  reg:broadcast(Cities).

update() ->
  Cities = [montreal, toronto, nyc, losangeles, mexico, iztapalapa],
  reg:update(Cities).
