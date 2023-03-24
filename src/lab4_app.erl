-module(lab4_app).

-behaviour(application).

-export([start/2, stop/1]).

-import(chat_server, [init/1, terminate/2]).

start(_StartType, _StartArgs) ->
  chat_server:start(),
  ok.


stop(_State) ->
  chat_server:terminate("Stop", "Exit"),
  ok.
