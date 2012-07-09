%% @doc Autoexporting parse transform.

-module(emark_parse_transform).

-export([ parse_transform/2
        ]).

-include("emark_internal.hrl").

%% @doc Main parse transform function.
-spec parse_transform(list(any()), list({ atom(), any() })) ->
                         list(any()).
parse_transform(Forms, Options) ->
  Suffix = emark_utils:fun_suffix(Options),
  %% get all exported functions including those with "_benchmark" suffix
  Exports = emark_utils:exports_of_forms(Forms, Suffix),

  transform(Forms, Exports, Suffix).

%===============================================================================

%% @doc Change the list of exported functions.
funcs_cons([ { Func, ?BENCH_FUN_ARITY } | Funcs ]) ->
  { cons, 0
  , { 'fun', 0
    , { function, Func, ?BENCH_FUN_ARITY }
    }
  , funcs_cons(Funcs)
  };
funcs_cons([]) ->
  { nil, 0 }.

%% @doc Change module declaration (add benchmark functions).
%% Also add benchmark/0 function.
module_autoexport(M, Forms, Exports, Suffix) ->
  ExportsList = sets:to_list(Exports),

  %% get benchmark functions
  Funcs = lists:filter(fun({ Name, _ }) ->
                           lists:suffix(Suffix, atom_to_list(Name))
                       end,
                       ExportsList),

  rebar_log:log(debug, "Benchmark funcs: ~p~n", [ Funcs ]),

  %% construct benchmark/0 function
  Benchmark = { function, 0, benchmark, 0
              , [ { clause, 0, [], []
                  , [ funcs_cons(Funcs) ]
                  }
                ]
              },

  Ex = { attribute, 0, export, [ { benchmark, 0 } | ExportsList ] },

  [ M, Ex, Benchmark | remove_exports(Forms) ].

%% @doc Filter all exports.
remove_exports([ { attribute, _, export, _ } | Forms ]) ->
  remove_exports(Forms);
remove_exports([ Form | Forms ]) ->
  [ Form | Forms ];
remove_exports([]) ->
  [].

%% @doc Call module_decl on every module declaration.
transform([ { attribute, _, module, _ } = MDecl | Forms ], Exports, Suffix) ->
  module_autoexport(MDecl, Forms, Exports, Suffix);
transform([ Form | Forms ], Exports, Suffix) ->
  [ Form | transform(Forms, Exports, Suffix) ];
transform([], _Exports, _Suffix) ->
  [].

%%% Local Variables:
%%% erlang-indent-level: 2
%%% End:
