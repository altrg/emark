%% @doc Report generation, loading and diff.

-module(emark_report).

-export([ from_file/1
        , to_file/2
        , to_stdout/1
        , show_diff/2
        ]).

%% format strings
-define(fmt_func,  "~-32s").  % function/arity
-define(fmt_time,  "~14.1f"). % us
-define(fmt_na,    "~14s").   % "n/a"
-define(fmt_delta, "~14s~n"). % delta
-define(fmt_iter,  "~14B").   % number of iterations

%% @doc Load report from a file.
from_file(Filename) ->
  try
    { ok, Data } = file:read_file(Filename),

    F = fun([ Func, Arity, Count, Time ]) ->
            { list_to_atom(Func)
              , list_to_integer(Arity)
              , list_to_integer(Count)
              , list_to_float(Time)
            }
        end,

    case re:run(Data, "\([^/]+\)/([0-9]+)[ ]+([^ ]+)[ ]+([^ ]+)[^\n]+\n",
                [ global, { capture, [ 1, 2, 3, 4 ], list }, unicode ]) of
      { match, Keys } ->
        lists:map(F, Keys);

      nomatch ->
        { error, invalid_report }
    end

  catch
    _:_ ->
      { error, failed }
  end.

%% @doc Save report to a file.
to_file(Report, Filename) ->
  file:write_file(Filename, to_string(Report)).

%% @doc Dump report to stdout.
to_stdout(Report) ->
  io:format("~s", [ to_string(Report) ]).

%% @doc Convert report into a readable form.
to_string(Report) ->
  F = fun({ Func, Arity, Count, Average }) ->
          io_lib:format(?fmt_func ?fmt_iter ?fmt_time " us/op~n",
                        [ func_to_string(Func, Arity), Count, Average ])
      end,

  lists:flatmap(F, Report).

%% @doc Dump difference between two reports to stdout.
show_diff(Old0, New0) ->
  Map = fun(Mark, List) ->
            L = lists:map(fun({ F, A, _C, T0 }) ->
                              Key = func_to_string(F, A),
                              T = trunc(T0 * 10.0) / 10.0,
                              { Key, { T, Mark } }
                          end,
                          List),
            dict:from_list(L)
        end,

  Old = Map(old, Old0),
  New = Map(new, New0),

  Diff = dict:merge(fun(_Key, { OldT, old }, { NewT, new }) ->
                        { OldT, NewT }
                    end,
                    Old,
                    New),

  diff_to_stdout(Diff).

%===============================================================================

func_to_string(F, A) ->
  atom_to_list(F) ++ "/" ++ integer_to_list(A).

diff_to_stdout(Diff) ->
  Delta = fun(X, Y) ->
              Change = 100 * (1.0 - (X / Y)),
              io_lib:format(case X > Y of
                              true  -> "~.2f%";
                              false -> "+~.2f%"
                            end,
                            [ Change ])
          end,

  NA = "n/a",

  io:format(?fmt_func ?fmt_na ?fmt_na ?fmt_delta,
            [ "benchmark", "old us/op", "new us/op", "delta" ]),

  lists:foreach(fun({ Func, { T, new } }) ->
                    io:format(?fmt_func ?fmt_na ?fmt_time ?fmt_delta,
                              [ Func, NA, T, NA ]);
                   ({ Func, { T, old } }) ->
                    io:format(?fmt_func ?fmt_time ?fmt_na ?fmt_delta,
                              [ Func, T, NA, NA ]);
                   ({ Func, { OldT, NewT }}) ->
                    io:format(?fmt_func ?fmt_time ?fmt_time ?fmt_delta,
                              [ Func, OldT, NewT, Delta(OldT, NewT) ])
                end,
                lists:sort(dict:to_list(Diff))).

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
