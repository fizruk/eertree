/*
* Google benchmark
* https://github.com/google/benchmark
*/
#define BENCH_RUN
#include "./queries/queries.h"
#include <benchmark/benchmark.h>

#include <iostream>
#include <ctime>
#include <unistd.h>

#define BENCH_N 2000
#define ITER_N 10

static std::string randomQueries(const int len)
{

    std::string s;
    // - is for deleting the last inserted character
    static const std::string alphaFull = "abcdefghijklmnopqrstuvwxyz-";

    // Select alphabet size here
    static const std::string alpha = alphaFull;

    srand((unsigned)time(NULL) * getpid());

    s.reserve(len);
    int real_count = 0;
    for (int i = 0; i < len; ++i)
    {
        int x = rand() % (sizeof(alpha)/8);
        while(x == 26 && !real_count)
        {
            x = rand() % (sizeof(alpha)/8);
        }
        if(x != 26)
        {
            real_count ++;
        }
        else
        {
            real_count --;
        }
        s += alpha[x];
    }

    return s;
}

static void bench(benchmark::State &state)
{
    int len = state.range(0);

    std::string s = randomQueries(len);
    for (auto _ : state)
    {
        process_queries(len, s);
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
