-module(curling_scoreboard_hw).
-export([ add_point/1, next_round/0, set_teams/2, reset_board/0 ]).

%% This is a 'dumb' module that's only there to replace what a real hardware
%% controller would likely do. The real hardware controller would likely hold
%% some state and make sure everythimg works right, but this one doesn't mind.

%% Shows the teams on the scoreboard.
set_teams(TeamA, TeamB) ->
    io:format("Scoreboard: Team ~s vs. Team ~s~n", [ TeamA, TeamB ]).

%% Next round.
next_round() ->
    io:format("Scoreboard: round over~n").

%% Increasing points for specified team.
add_point(Team) ->
    io:format("Scoreboard: increased score of team ~s by 1~n", [ Team ]).

%% Resets board (teams and scores).
reset_board() ->
    io:format("Scoreboard: All teams are undefined and all scores are 0~n").
