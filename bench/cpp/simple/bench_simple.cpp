/*
* Google benchmark
* https://github.com/google/benchmark
*/
#include <benchmark/benchmark.h>
#define ALPHA_LOWERCASE

#ifdef EERTREE_SEMI_PERS
#include "eertree_semi_pers.h" 
#elif defined(EERTREE_DIRECT)
#include "eertree_direct.h"
#else 
#include "eertree_classic.h"
#endif
#include <iostream>
#include <ctime>
#include <unistd.h>

#define BENCH_N 2000
#define ITER_N 10

static std::string randomString(const int len)
{
    /* https://stackoverflow.com/a/440240 */

    std::string s;

    static const std::string alphaFull = "abcdefghijklmnopqrstuvwxyz";

    // Select alphabet size here
    static const std::string alpha = alphaFull;
    int alpha_size = 26;
    srand((unsigned)time(NULL) * getpid());

    s.reserve(len);

    for (int i = 0; i < len; ++i)
    {
        s += alpha[rand() % alpha_size];
    }

    return s;
}

static void process_palindromes(const std::string s)
{
    EERTREE tree;
    for (int i = 0; i < s.size(); ++i)
    {
        // std::cout << i << '\n';
        tree.insert(s, i);
    }

    int result;
    tree.unique_palindromes_n(s, result);
}

static void bench(benchmark::State &state)
{
    int len = state.range(0);

    std::string s = randomString(len);

    for (auto _ : state)
    {
        process_palindromes(s);
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

// int main(int argc, char *argv[])
// {
//     int len = 10;

//     std::string s = randomString(len);

//     EERTREE tree;
//     for (int i = 0; i < s.size(); ++i)
//         tree.insert(s, i);

//     tree.printPalindromes(s);
// }
