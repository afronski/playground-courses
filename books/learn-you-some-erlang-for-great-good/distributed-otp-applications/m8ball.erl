-module(m8ball).
-behaviour(application).
-export([ start/2, stop/1 ]).
-export([ ask/1 ]).

start(normal, []) ->
    m8ball_sup:start_link();
start({ takeover, _OtherNode }, []) ->
    m8ball_sup:start_link().

stop(_State) ->
    ok.

ask(Question) ->
    m8ball_server:ask(Question).
