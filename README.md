emark
=====

Lightweight benchmarking framework for Erlang.

It is a plugin for [rebar](https://github.com/basho/rebar) which
benchmarks your code.

What it looks like
==================

```
==> bench0 (emark)
calc_something/0	20000	38.1 µs/op
parse_omg_wtf/1	500000	1.6 µs/op

benchmark                      old µs/op new µs/op   delta
calc_something/0                    37.7      38.0  +0.79%
parse_omg_wtf/1                      1.6       1.5  -6.67%
```

Usage example
=============

There is an example in `examples/bench0` subdirectory.

    cd examples/bench0
    mkdir -p deps
    ln -s `pwd`/../../ deps/emark
    ../../rebar compile
    ../../rebar emark

Details
=======

**emark** works almost like **eunit**.

```erlang
-include_lib("emark/include/emark.hrl").

-ifdef(BENCHMARK).

my_function_benchmark(N) ->
  Input = prepare_n_inputs(N),
  emark:start(?MODULE, my_function, 1),
  lists:seq(fun(E) -> _ = my_function(E) end, Input).

-endif.
```

The main difference is the `emark:start` call. It starts tracing the
specified function, so it then knows how many times it was really
called while benchmark was run. It also starts the actual timer, so
preparation should be done before calling it.

**emark** prints benchmark results to the terminal. First column is
the name of the function and its arity, second column is the number of
iterations during the benchmark, thirs column is the average time it
took the function to run (microseconds).

By default, **emark** also saves a report to a file under `.emark/`
directory. It's used on next run to show a difference between runs.
So the idea behind **emark** is that you run benchmark (`rebar
emark`), then change the code of some important function, and rerun
benchmark to see if anything went better (or worse).
