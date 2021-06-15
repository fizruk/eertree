/*
* Google benchmark
* https://github.com/google/benchmark
*/
#include <benchmark/benchmark.h>
#include "eertree.cpp"

#include <iostream>
#include <ctime>
#include <unistd.h>

#define BENCH_N 10000
#define ITER_N 100

static std::string randomString(const int len);
static void palindromes();

static std::string randomString(const int len)
{
    /* https://stackoverflow.com/a/440240 */

    std::string s;

    static const std::string alphaFull = "abcdefghijklmnopqrstuvwxyz";

    // Select alphabet size here
    static const std::string alpha = alphaFull.substr(0, 4);

    srand((unsigned)time(NULL) * getpid());

    s.reserve(len);

    for (int i = 0; i < len; ++i)
    {
        s += alpha[rand() % (sizeof(alpha) / 8)];
    }

    return s;
}

static void palindromes(const std::string s)
{
    EERTREE tree;
    for (int i = 0; i < s.size(); ++i)
        tree.insert(s, i);

    std::string result;
    tree.palindromes(s, result);
}

static void bench(benchmark::State &state)
{
    int len = state.range(0);

    std::string s = randomString(len);

    for (auto _ : state)
    {
        palindromes(s);
    }
}

BENCHMARK(bench)
    // String lengths
    ->Arg(BENCH_N)
    ->Arg(2 * BENCH_N)
    ->Arg(4 * BENCH_N)
    ->Arg(8 * BENCH_N)
    ->Arg(16 * BENCH_N)
    ->Iterations(ITER_N)        // # of iterations
    ->Unit(benchmark::kSecond); // time in seconds

BENCHMARK_MAIN();

/*
Run benchmarks:
g++ bench.cpp -std=c++11 -isystem ../../../benchmark/include -Lbenchmark/build/src -lbenchmark -lpthread
./a.out
*/

// int main(int argc, char *argv[])
// {
//     int len = 10;

//     std::string s = randomString(len);

//     EERTREE tree;
//     for (int i = 0; i < s.size(); ++i)
//         tree.insert(s, i);

//     tree.printPalindromes(s);
// }
