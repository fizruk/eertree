#define BENCH_RUN
#include <benchmark/benchmark.h>
#ifndef RICH_SIMPLE
#include "./rich/rich.h"
#else
#include "./rich/rich_simple.h"
#endif
#include <iostream>
#include <ctime>
#include <unistd.h>

#define BENCH_N 2
#define ITER_N 10


static void bench(benchmark::State &state)
{
    int len = state.range(0);
    
    for (auto _ : state)
    {
         compute_rich_strings_of(len);
    }
}

BENCHMARK(bench)
    // String lengths
    ->Arg(1 * BENCH_N)
    ->Arg(2 * BENCH_N)
    ->Arg(3 * BENCH_N)
    ->Arg(4 * BENCH_N)
    ->Arg(5 * BENCH_N)
    ->Arg(6 * BENCH_N)
    ->Arg(7 * BENCH_N)
    ->Arg(8 * BENCH_N)
    ->Arg(9 * BENCH_N)
    ->Arg(10 * BENCH_N)
    ->Arg(11 * BENCH_N)
    ->Repetitions(ITER_N)
    ->ReportAggregatesOnly(true)
    ->Unit(benchmark::kSecond); // time in seconds

BENCHMARK_MAIN();
