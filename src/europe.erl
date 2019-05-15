-module(europe).
-author("Achil").

%% API
-export([connect/0, stop/0, start/0, update/0, broadcast/0]).

stop() ->
  Cities = [london, birminghan, hamburg, berlin, petersburg, moscow],
  reg:stop(Cities).

start() ->
  Cities = [london, birminghan, hamburg, berlin, petersburg, moscow],
  reg:start(Cities).

connect() ->
  Connections = [
    {london, [{birminghan, europe}, {berlin, europe}]},
    {birminghan, [{london, europe}]},
    {hamburg, [{berlin, europe}]},
    {berlin, [{hamburg, europe},{london, europe}, {moscow, europe}, {cairo, africa}, {nyc, america}]},
    {petersburg, [{moscow, europe}]},
    {moscow, [{petersburg, europe}, {berlin, europe}, {mumbai, asia}]}
  ],
  reg:connect(Connections).

broadcast() ->
  Cities = [london, birminghan, hamburg, berlin, petersburg, moscow],
  reg:broadcast(Cities).

update() ->
  Cities = [london, birminghan, hamburg, berlin, petersburg, moscow],
  reg:update(Cities).