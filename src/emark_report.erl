%% @doc Report generation, loading and diff.

-module(emark_report).

-export([ from_file/1
        , to_file/2
        , to_stdout/1
        , show_diff/2
        ]).

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

    case re:run(Data, "\([^/]+\)/([0-9]+)\t([^ \t]+)\t([^ \t]+)[^\n]+\n",
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
          io_lib:format("~p/~p\t~p\t~.1f µs/op~n",
                        [ Func, Arity, Count, Average ])
      end,

  lists:flatmap(F, Report).

%% @doc Dump difference between two reports to stdout.
show_diff(Old0, New0) ->
  Old = lists:sort(Old0),
  New = lists:sort(New0),

  Delta = fun(X, Y) ->
              Diff = 100 * (1.0 - (X / Y)),
              io_lib:format(case X > Y of
                              true  -> "~.2f%";
                              false -> "+~.2f%"
                            end,
                            [ Diff ])
          end,

  Cmp = fun({ F, A, _OldC, OldT }, { F, A, _NewC, NewT }) ->
            X = trunc(OldT * 10.0) / 10.0,
            Y = trunc(NewT * 10.0) / 10.0,
            Func = lists:flatten(io_lib:format("~p/~B", [ F, A ])),
            io:format("~-30s\t~10.1f\t~10.1f\t~8s~n",
                      [ Func, X, Y, Delta(X, Y) ])
        end,

  io:format("~-30s\t~11s\t~11s\t~8s~n",
            [ "benchmark", "old µs/op", "new µs/op", "delta" ]),
  lists:zipwith(Cmp, Old, New),
  ok.

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
