%% @doc Benchmarking.

-module(emark_benchmark).

-export([ run/2
        ]).

-include("emark_internal.hrl").

%% @doc Benchmark a specific modules.
run(Modules, Options) ->
  [ N
  ] = emark_utils:options(Options,
                          [ { n, ?BENCH_DEFAULT_N }
                          ]),

  RunModule = fun(M) ->
                  rebar_log:log(debug,
                                "Benchmarking ~p, ~p iterations~n",
                                [ M, N ]),
                  lists:map(fun(B) ->
                                run_func(M, B, N)
                            end,
                            M:benchmark())
              end,

  F = fun(M) ->
          { module, M } = code:load_file(M),
          case erlang:function_exported(M, benchmark, 0) of
            true ->
              RunModule(M);
            false ->
              [ ignore ]
          end
      end,

  lists:filter(fun(R) -> R /= ignore end,
               lists:flatmap(F, Modules)).

%===============================================================================

%% @doc Get the call count of a specific function.
get_call_count(MFA) ->
  { call_count, Count } = erlang:trace_info(MFA, call_count),
  Count.

%% @doc Map benchmark to its results.
run_func(_M, B, N) ->
  { { _Mod, Func, Arity }, Count, Time } = trace(B, N),
  { Func, Arity, Count, Time/Count }.

%% @doc Run the benchmark function.
%% It also sends the function MFA and time it took to execute
%% back to the callee.
trace(B, N) ->
  Self = self(),
  spawn(fun() ->
            %% get the time it takes to run
            StartTime = os:timestamp(),
            B(N),
            EndTime = os:timestamp(),
            Time = timer:now_diff(EndTime, StartTime),
            %% stop tracing
            erlang:trace(all, false, [ all ]),
            %% send the MFA we were benchmarking
            receive
              MFA = { function, { _, _, _ } } ->
                Self ! MFA
            end,
            %% send the real timings
            receive
              { started, StartReal } ->
                Self ! { finished, Time - timer:now_diff(StartReal, StartTime) }
            end
        end),

  trace_loop(B, N, undefined).

%% @doc Benchmark callee.
%% It executes the benchmark until the time it took is enough
%% (?BENCH_DEFAULT_TIME) and number of iterations is meaningful.
trace_loop(B, N, MFA) ->
  receive
    { function, NewMFA } ->
      trace_loop(B, N, NewMFA);

    { finished, Time } ->
      rebar_log:log(debug, "finished benchmark~n", []),
      Count = get_call_count(MFA),

      case Time of
        X when X < ?BENCH_DEFAULT_TIME ->
          %% if it's less, we should try a better number of iterations
          rebar_log:log(debug, "not enough time (~p ms vs ~p ms)~n",
                        [ trunc(X / 1000), trunc(?BENCH_DEFAULT_TIME / 1000) ]),
          Average = Time / Count,
          %% woooo.... holy crap X___x
          %% yeah, put some cool stuff here later
          NeedCount = emark_utils:ceil((?BENCH_DEFAULT_TIME * 1.05) / Average),
          %% restart the benchmark
          rebar_log:log(debug,
                        "restaring the benchmark with ~p iterations~n",
                        [ NeedCount ]),
          trace(B, NeedCount);

        _ ->
          { MFA, Count, Time }
      end;

    _ ->
      trace_loop(B, N, MFA)
  end.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
