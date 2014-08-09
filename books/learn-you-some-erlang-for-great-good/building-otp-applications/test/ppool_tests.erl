-module(ppool_tests).
-include_lib("eunit/include/eunit.hrl").
-export([ test_mfa/1, wait_mfa/1 ]).

start_test_() ->
    { "It should be possible to start a pool server and give it a name",
      { setup,
        fun find_unique_name/0,
        fun(Name) -> [ start_and_test_name(Name) ] end
      } 
    }.

mfa_test_() ->
    { "A pool process can be allocated which will be ordered "
      "to run an MFA call determined at start time, with arguments "
      "provided at call time",
      { setup,
        fun start_ppool/0,
        fun kill_ppool/1,
        fun(Name) -> [ pool_run_mfa(Name) ] end
      }
    }.

alloc_test_() ->
    { "A pool process can be allocated which will be ordered "
      "to run a worker, only if there are enough which "
      "haven't been ordered to run yet.",
      { setup,
        fun start_ppool/0,
        fun kill_ppool/1,
        fun(Name) -> [
            pool_run_alloc(Name),
            pool_run_noalloc(Name)
        ]
        end
      }
    }.

realloc_test_() ->
    { "When an allocated process dies, "
      "A new one can be allocated to replace it.",
      { setup,
        fun start_ppool/0,
        fun kill_ppool/1,
        fun(Name) -> [ pool_run_realloc(Name) ] end
      }
    }.

queue_test_() ->
    { "The queue function can be used to run the function as soon as possible. "
      "If no space is available, the worker call is added to the queue.",
      { foreach,
        fun start_ppool/0,
        fun kill_ppool/1,
        [ fun(Name) -> test_async_queue(Name) end,
          fun(Name) -> test_sync_queue(Name) end
        ]
      }
    }.

supervision_test_() ->
    { "The ppool will never restart a dead child, but all children (OTP "
      "compliant) will be shut down when closing the pool, even if they "
      "are trapping exits",
      { setup,
        fun find_unique_name/0,
        fun test_supervision/1
      }
    }.
    
auth_test_() ->
    { "The ppool should only dequeue tasks after receiving a down signal "
      "from a worker and nobody else",
      { setup,
        fun start_ppool/0,
        fun kill_ppool/1,
        fun test_auth_dealloc/1
      }
    }.

%%%
%%% Setup / teardown
%%%
find_unique_name() ->
    application:start(ppool),
    Name = list_to_atom(lists:flatten(io_lib:format("~p", [ now() ]))),
    ?assertEqual(undefined, whereis(Name)),
    Name.

start_ppool() ->
    Name = find_unique_name(),
    ppool:start_pool(Name, 2, { ppool_nagger, start_link, [] }),
    Name.

kill_ppool(Name) ->
    ppool:stop_pool(Name).
    
%%% Actual tests
start_and_test_name(Name) ->
    ppool:start_pool(Name, 1, { ppool_nagger, start_link, [] }),
    A = whereis(Name),

    ppool:stop_pool(Name),
    timer:sleep(100),

    B = whereis(Name),

    [ ?_assert(undefined =/= A), 
      ?_assertEqual(undefined, B) ].

pool_run_mfa(Name) ->
    ppool:run(Name, [ i_am_running, 1, 1, self() ]),
    X = receive
        { _Pid, i_am_running } -> ok
    after 3000 ->
        timeout
    end,
    ?_assertEqual(ok, X).

pool_run_alloc(Name) ->
    { ok, Pid } = ppool:run(Name, [ i_am_running, 1, 1, self() ]),
    X = receive
        { Pid, i_am_running } -> ok
    after 3000 ->
        timeout
    end,
    [ ?_assert(is_pid(Pid)),
      ?_assertEqual(ok, X) ].

pool_run_noalloc(Name) ->
    %% Init function should have set the limit to 2.
    ppool:run(Name, [ i_am_running, 300, 1, self() ]),
    ppool:run(Name, [ i_am_running, 300, 1, self() ]),

    X = ppool:run(Name, [ i_am_running, 1, 1, self() ]),

    ?_assertEqual(noalloc, X).
    
pool_run_realloc(Name) ->
    %% Init function should have set the limit to 2.
    { ok, A } = ppool:run(Name, [ i_am_running, 500, 1, self() ]),
    timer:sleep(100),

    {ok, B} = ppool:run(Name, [ i_am_running, 500, 1, self() ]),
    timer:sleep(600),

    { ok, Pid } = ppool:run(Name, [ i_am_running, 1, 1, self() ]),
    timer:sleep(100),

    L = flush(),

    [ ?_assert(is_pid(Pid)),
      ?_assertEqual([ { A, i_am_running }, 
                      { B, i_am_running }, 
                      { Pid, i_am_running }
                    ], L) 
    ].

test_async_queue(Name) ->
    %% Still two elements max!
    ok = ppool:async_queue(Name, [ i_am_running, 2000, 1, self() ]),
    ok = ppool:async_queue(Name, [ i_am_running, 2000, 1, self() ]),

    noalloc = ppool:run(Name, [ i_am_running, 2000, 1, self() ]),
    ok = ppool:async_queue(Name, [ i_am_running, 500, 1, self() ]),
    timer:sleep(3500),

    L = flush(),

    ?_assertMatch([ { _, i_am_running }, { _, i_am_running }, { _, i_am_running } ], L).

test_sync_queue(Name) ->
    { ok, Pid } = ppool:sync_queue(Name, [ i_am_running, 200, 1, self() ]),

    ok = ppool:async_queue(Name, [ i_am_running, 200, 1, self() ]),
    ok = ppool:async_queue(Name, [ i_am_running, 200, 1, self() ]),

    { ok, Pid2 } = ppool:sync_queue(Name, [ i_am_running, 100, 1, self() ]),

    timer:sleep(300),

    L = flush(),

    [ ?_assert(is_pid(Pid)),
      ?_assert(is_pid(Pid2)),
      ?_assertMatch([ { _, i_am_running }, { _, i_am_running },
                      { _, i_am_running }, { _, i_am_running }
                    ], L) 
    ].

test_supervision(Name) ->
    ppool:start_pool(Name, 1, { ppool_nagger, start_link, [] }),
    { ok, Pid } = ppool:run(Name, [ sup, 10000, 100, self() ]),

    ppool:stop_pool(Name),
    timer:sleep(100),

    ?_assertEqual(undefined, process_info(Pid)). 

test_auth_dealloc(Name) ->
    { ok, _Pid } = ppool:sync_queue(Name, [ i_am_running, 500, 1, self() ]),

    ok = ppool:async_queue(Name, [ i_am_running, 10000, 1, self() ]),
    ok = ppool:async_queue(Name, [ i_am_running, 10000, 1, self() ]),
    ok = ppool:async_queue(Name, [ i_am_running, 1, 1, self() ]),

    timer:sleep(600),

    Name ! { 'DOWN', make_ref(), process, self(), normal },
    Name ! { 'DOWN', make_ref(), process, self(), normal },
    Name ! { 'DOWN', make_ref(), process, self(), normal },

    timer:sleep(200),
    L = flush(),

    ?_assertMatch([ { _, i_am_running } ], L).
    
flush() ->
    receive
        X -> [ X | flush() ]
    after 0 ->
        []
    end.

%%
%% Exported helper functions.
%%
test_mfa(Pid) ->
    Pid ! i_am_running.

wait_mfa(Pid) ->
    Pid ! i_am_running,
    timer:sleep(3000).
