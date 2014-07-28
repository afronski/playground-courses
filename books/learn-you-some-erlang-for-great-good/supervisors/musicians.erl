-module(musicians).
-behaviour(gen_server).

-export([ start_link/2, stop/1 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2 ]).

-record(state, { name = "", role, skill = good }).
-define(DELAY, 750).

start_link(Role, Skill) ->
    gen_server:start_link({ local, Role }, ?MODULE, [ Role, Skill ], []).

stop(Role) ->
    gen_server:call(Role, stop).

init([ Role, Skill ]) ->
    %% To know when parent shuts down.
    process_flag(trap_exit, true),
    
    %% Sets a seed for random number generation for the life of the process
    %% uses the current time to do it. Unique value guaranteed by now().
    random:seed(now()),
    TimeToPlay = random:uniform(3000),
    Name = pick_name(),
    StrRole = atom_to_list(Role),
    io:format("Musician ~s, playing the ~s entered the room~n",
              [ Name, StrRole ]),
    { ok, #state{ name = Name, role = StrRole, skill = Skill }, TimeToPlay }.

%% Yes, the names are based off the magic school bus characters'
%% 10 names!
pick_name() ->
    %% The seed must be set for the random function. Use within the
    %% process that started with init/1.
    lists:nth(random:uniform(10), firstnames())
    ++ " " ++
    lists:nth(random:uniform(10), lastnames()).

firstnames() ->
    [ "Valerie", "Arnold", "Carlos", "Dorothy", "Keesha",
      "Phoebe", "Ralphie", "Tim", "Wanda", "Janet" ].

lastnames() ->
    [ "Frizzle", "Perlstein", "Ramon", "Ann", "Franklin",
      "Terese", "Tennelli", "Jamal", "Li", "Perlstein" ].

handle_call(stop, _From, S = #state{}) ->
    { stop, normal, ok, S };
handle_call(_Message, _From, S) ->
    { noreply, S, ?DELAY }.

handle_cast(_Message, S) ->
    { noreply, S, ?DELAY }.

handle_info(timeout, S = #state{ name = N, skill = good }) ->
    io:format("~s produced sound!~n", [ N ]),
    { noreply, S, ?DELAY };
handle_info(timeout, S = #state{ name = N, skill = bad }) ->
    case random:uniform(5) of
        1 ->
            io:format("~s played a false note. Uh oh~n", [ N ]),
            { stop, bad_note, S };
        _ ->
            io:format("~s produced sound!~n", [ N ]),
            { noreply, S, ?DELAY }
    end;
handle_info(_Message, S) ->
    { noreply, S, ?DELAY }.

code_change(_OldVsn, State, _Extra) ->
    { ok, State }.

terminate(normal, S) ->
    io:format("~s left the room (~s)~n", [ S#state.name, S#state.role ]);
terminate(bad_note, S) ->
    io:format("~s sucks! Kicked that member out of the band! (~s)~n",
              [ S#state.name, S#state.role ]);
terminate(shutdown, S) ->
    io:format("The manager is mad and fired the whole band! "
              "~s just got back to playing in the subway~n",
              [ S#state.name ]);
terminate(_Reason, S) ->
    io:format("~s has been kicked out (~s)~n", [ S#state.name, S#state.role ]).
