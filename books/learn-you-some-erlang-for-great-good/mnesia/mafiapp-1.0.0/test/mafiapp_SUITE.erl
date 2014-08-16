-module(mafiapp_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([ init_per_suite/1, end_per_suite/1, 
          init_per_testcase/2, end_per_tesccase/2,
          all/0 ]).
-export([ add_service/1 ]).

all() ->
    [ add_service ].

init_per_suite(Config) ->
    Priv = ?config(priv_dir, Config),
    application:set_env(mnesia, dir, Priv),
    mafapp:install([ node() ]),
    application:start(mnesia),
    application:start(mafiapp),
    Config.

end_per_suite(_Config) ->
    application:stop(mnesia),
    ok.

init_per_testcase(add_service, Config) ->
    Config.

end_per_testcase(_, Config) ->
    ok.

%% Services can go both way: from a friend to the boss, or
%% from the boss to a friend! A boss friend is required!
add_service(_Config) ->
    { error, unknown_friend } = mafiapp:add_service("from name",
                                                    "to name",
                                                    { 1946, 5, 23 },
                                                    "a fake service"),
    ok = mafiapp:add_friend("Don Corleone", [], [ boss ], boss),
    ok = mafiapp:add_friend("Alans Parsons",
                            [ { twitter, "@ArtScienceSound" } ],
                            [ { born, { 1948, 12, 20 } },
                              musician, 'audio engineer',
                              producer, "has projects" ],
                            mixing),
    ok = mafiapp:add_service("Alan Parsons", "Don Corleone",
                             { 1973, 3, 1 },
                             "Helped release a Pink Floyd album.").
    
