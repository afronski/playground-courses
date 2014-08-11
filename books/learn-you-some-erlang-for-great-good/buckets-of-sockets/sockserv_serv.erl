-module(sockserv_serv).
-behaviour(gen_server).

-record(state, { name,      % Player's name.
                 next,      % Next step, used when initializing.
                 socket }). % The current socket.

-export([ start_link/1 ]).
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2,
          code_change/3, terminate/2 ]).

-define(TIME, 800).
-define(EXP, 50).

line(Str) ->
    hd(string:tokens(Str, "\r\n ")).

send(Socket, Str, Args) ->
    ok = gen_tcp:send(Socket, io_lib:format(Str ++ "~n", Args)),
    ok = inet:setopts(Socket, [ { active, once } ]),
    ok.

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
    %% Properly seed the process.
    << A:32, B:32, C:32 >> = crypto:rand_bytes(12),
    random:seed({ A, B, C }),
    
    %% Because accepting a connection is a blocking function call,
    %% we can not do it in here. Forward to the server loop!
    gen_server:cast(self(), accept),
    { ok, #state{ socket = Socket } }.

%% We never need you, handle_call!
handle_call(_E, _From, State) ->
    { noreply, State }.

handle_cast(accept, S = #state{ socket = ListenSocket }) ->
    { ok, AcceptSocket } = gen_tcp:accept(ListenSocket),
    
    %% Remember that thou art dust, and to dust thou shalt return.
    %% We want to always keep a given number of children in this app.
    sockserv_sup:start_socket(),
    send(AcceptSocket, "What's your character's name?", []),
    { noreply, S#state{ socket = AcceptSocket, next = name } };

handle_cast(roll_stats, S = #state { socket = Socket }) ->
    Roll = pq_stats:initial_roll(),
    send(Socket,
         "Stats for your character:~n"
         "  Charisma: ~B~n"
         "  Constitiution: ~B~n"
         "  Dexterity: ~B~n"
         "  Intelligence: ~B~n"
         "  Strength: ~B~n"
         "  Wisdom: ~B~n~n"
         "Do you agree to these? y/n~n",
         [ Points || { _Name, Points } <- lists:sort(Roll) ]),
    { noreply, S#state{ next = { stats, Roll } } };
handle_cast(stats_accepted, S = #state{ name = Name, next = { stats, Stats } }) ->
    processquest:start_player(Name, [ { stats, Stats }, { time, ?TIME },
                                      { lvlexp, ?EXP } ]),
    processquest:subscribe(Name, sockserv_pq_events, self()),
    { noreply, S#state{ next = playing } };

%% Events coming from process quest.
%% We know this because all these events' tuples start with the
%% name of the player as part of internal protocol defined for us.
handle_cast(Event, S = #state { name = N, socket = Sock }) when element(1, Event) =:= N ->
    [ case E of
          { wait, Time } -> timer:sleep(Time);
          IoList -> send(Sock, IoList, [])
      end || E <- sockserv_trans:to_str(Event) ],
    { noreply, S }.

handle_info({ tcp, _Socket, _Str }, S = #state{ next = name }) ->
    Name = line(str),
    gen_server:cast(self(), roll_stats),
    { noreply, S#state{ name = Name, next = stats } };
handle_info({ tcp, Socket, Str }, S = #state{ socket = Socket, next = { stats, _ } }) ->
    case line(Str) of
        "y" -> gen_server:cast(self(), stats_accepted);
        "n" -> gen_server:cast(self(), roll_stats);
        _ -> send(Socket, "Answer with y (yes) or n (no)", [])
    end,
    { noreply, S };
handle_info({ tcp, _Socket, "quit" ++ _}, S) ->
    processquest:stop_player(S#state.name),
    gen_tcp:close(S#state.socket),
    { stop, normal, S };
handle_info({ tcp_closed, _Socket}, S) ->
    { stop, normal, S };
handle_info({ tcp_error, _Socket, _ }, S) ->
    { stop, normal, S };
handle_info(E, S) ->
    io:format("Unexpected: ~p~n", [ E ]),
    { noreply, S }.

code_change(_OldVsn, State, _Extra) ->
    { ok, State }.

terminate(normal, _State) ->
    ok;
terminate(_Reason, _State) ->
    io:format("Terminate reason: ~p~n", [ _Reason ]).
