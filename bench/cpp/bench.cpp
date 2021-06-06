/*
Google benchmark
https://github.com/google/benchmark
*/
#include <benchmark/benchmark.h>
#include "eertree.cpp"

#include <iostream>
#include <ctime>
#include <unistd.h>

static std::string randomString(const int len);
static void palindromes();

static std::string randomString(const int len)
{
    /* https://stackoverflow.com/a/440240 */

    std::string s;

    static const std::string alpha2 = "ab";
    static const std::string alpha4 = "abcd";
    static const std::string alphaFull = "abcdefghijklmnopqrstuvwxyz";

    static const std::string alpha = alpha4;

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

    std::string res = tree.palindromesAll(s);
}

static void bench(benchmark::State &state)
{
    static const int LEN[5] = {10000, 20000, 40000, 80000, 160000};

    // Change index for different lengths
    std::string s = randomString(LEN[0]);

    for (auto _ : state)
    {
        palindromes(s);
    }
}

BENCHMARK(bench);

BENCHMARK_MAIN();

/*
Run benchmarks
g++ bench.cpp -std=c++11 -isystem ../../../benchmark/include -Lbenchmark/build/src -lbenchmark -lpthread
*/

// int main(int argc, char *argv[])
// {
//    int len = 10;
// 
//     std::string s = randomString(len);
// 
//     EERTREE tree;
//     for (int i = 0; i < s.size(); ++i)
//         tree.insert(s, i);
//     
//     std::cout << tree.palindromesAll(s) << std::endl;
// }
