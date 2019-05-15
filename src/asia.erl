-module(asia).
-author("Achil").

%% API
-export([connect/0, stop/0, start/0, update/0, broadcast/0]).

stop() ->
  Cities = [guangzhou, shangahai, delhi, mumbai, jakarta, surabaya],
  reg:stop(Cities).

start() ->
  Cities = [guangzhou, shangahai, delhi, mumbai, jakarta, surabaya],
  reg:start(Cities).

connect() ->
  Connections = [
    {guangzhou, [{shangahai, asia}]},
    {shangahai, [{guangzhou, asia}, {mumbai, asia}]},
    {delhi, [{mumbai, asia}]},
    {mumbai, [{delhi, asia},{jakarta, asia}, {moscow, europe}, {cairo, africa}]},
    {jakarta, [{surabaya, asia}, {mumbai, asia}]},
    {surabaya, [{jakarta, asia}]}
  ],
  reg:connect(Connections).

broadcast() ->
  Cities = [guangzhou, shangahai, delhi, mumbai, jakarta, surabaya],
  reg:broadcast(Cities).

update() ->
  Cities = [guangzhou, shangahai, delhi, mumbai, jakarta, surabaya],
  reg:update(Cities).

