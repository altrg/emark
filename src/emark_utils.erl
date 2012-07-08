-module(emark_utils).

-export([ exports_of_forms/1
        , exports_of_forms/2
        , fun_suffix/1
        ]).

-include("emark_internal.hrl").

exports_of_forms(Forms) ->
  exports_of_forms(Forms, no_autoexport).

exports_of_forms(Forms, Suffix) ->
  F = fun({ attribute, _, export, ExList }, Accu) ->
          sets:union(sets:from_list(ExList), Accu);

         ({ function, _, Name, ?BENCH_FUN_ARITY, _ }, Accu)
          when is_list(Suffix) ->
          case lists:suffix(Suffix, atom_to_list(Name)) of
            true ->
              sets:add_element({ Name, ?BENCH_FUN_ARITY }, Accu);
            false ->
              Accu
          end;

         (_, Accu) ->
          Accu
      end,

  lists:foldl(F, sets:new(), Forms).

fun_suffix(Options) ->
  proplists:get_value(emark_fun_suffix, Options, ?DEFAULT_FUN_SUFFIX).
