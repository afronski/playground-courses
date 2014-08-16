-module(mafiapp).
-behaviour(application).
-include_lib("stdlib/include/ms_transform.hrl").
-export([ start/2, stop/1 ]).
-export([ install/1 ]).
-export([ add_friend/4, add_service/4, friend_by_name/1, friend_by_expertise/1 ]).

-record(mafiapp_friends, { name,
                           contact = [],
                           info = [],
                           expertise }).
-record(mafiapp_services, { from,
                            to,
                            date,
                            description }).

install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [ mnesia ]),
    mnesia:create_table(mafiapp_friends,
                        [ { attributes, record_info(fields, mafiapp_friends) },
                          { index, [ #mafiapp_friends.expertise ] },
                          { disc_copies, Nodes }
                        ]),
    mnesia:create_table(mafiapp_services,
                        [ { attributes, record_info(fields, mafiapp_services) },
                          { index, [ #mafiapp_services.to ] },
                          { disc_copies, Nodes },
                          { type, bag }
                        ]),
    rpc:multicall(Nodes, application, stop, [ mnesia ]).

start(normal, []) ->
    mnesia:wait_for_tables([ mafiapp_friends, mafiapp_services ], 5000),
    mafiapp_sup:start_link().

stop(_) ->
    ok.

add_friend(Name, Contact, Info, Expertise) ->
    F = fun() ->
                mnesia:write(#mafiapp_friends{ name = Name,
                                               contact = Contact,
                                               info = Info,
                                               expertise = Expertise })
        end,
    mnesia:activity(transaction, F).

add_service(From, To, Date, Description) ->
    F = fun() ->
                case mnesia:read({ mafiapp_friends, From }) =:= [] orelse
                     mnesia:read({ mafiapp_friends, To }) =:= [] of
                    true ->
                        { error, unknown_friend };
                    false ->
                        mnesia:write(#mafiapp_services{ from = From,
                                                        to = To,
                                                        date = Date,
                                                        description = Description })
                end
        end,
    mnesia:activity(transaction, F).

friend_by_name(Name) ->
    F = fun() ->
                case mnesia:read({ mafiapp_friends, Name }) of
                    [ #mafiapp_friends{ contact = C, info = I, expertise = E }] ->
                        { Name, C, I, E, find_services(Name) };
                    [] ->
                        undefined
                end
        end,
    mnesia:activity(transaction, F).

friend_by_expertise(Expertise) ->
    Pattern = #mafiapp_friends{ _ = '_',
                                expertise = Expertise},
    F = fun() ->
                Res = mnesia:match_object(Pattern),
                [ { Name, C, I, Expertise, find_services(Name) } ||
                    #mafiapp_friends{ name = Name,
                                      contact = C,
                                      info = I } <- Res ]
        end,
    mnesia:activity(transaction, F).

%%%
%%% Private functions.
%%%
find_services(Name) ->
    Match = ets:fun2ms(
              fun(#mafiapp_services{ from = From, to = To, date = D, description = Desc }) 
                    when From =:= Name ->
                      { to, To, D, Desc };
                 (#mafiapp_services{ from = From, to = To, date = D, description = Desc })
                     when To =:= Name ->
                      { from, From, D, Desc }
              end
            ),
    mnesia:select(mafiapp_services, Match).                    
