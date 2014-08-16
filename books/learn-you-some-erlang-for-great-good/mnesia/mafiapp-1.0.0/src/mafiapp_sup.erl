-module(mafiapp_sup).
-behaviour(supervisor).
-export([ start_link/0 ]).
-export([ init/1 ]).

start_link() ->
    supervisor:start_link(?MODULE, []).

%% This does absolutely nothing, only there to
%% allow to wait for tables.
init([]) ->
    { ok, { { one_for_one, 1, 1 }, [] } }.
