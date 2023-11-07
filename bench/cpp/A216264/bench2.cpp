/*
* Google benchmark
* https://github.com/google/benchmark
* This implementation does not use one tree.
* It constructs a new tree for every new string to check if it's a rich string.
*/
#include <benchmark/benchmark.h>
#include "../eertree2.h"

#include <iostream>
#include <ctime>
#include <unistd.h>

#define ITER_N 5
#define BENCH_N 2

static void palindromes();

static void AllStrings(std::vector<std::string>* strings, std::string str, const int len){
    if(str.size() == len){
        strings->push_back(str);
        return;
    }
    AllStrings(strings, str + "0", len);
    AllStrings(strings, str + "1", len);
}

// -- Observation: there is exactly the same number of rich strings
// -- that start with 0 as there are those starting with 1.
// -- That is why we can do half work (or 1/(alphabet size) in general)
// -- and count rich strings faster.
static void palindromes(std::vector<std::string>* strings, int len)
{
    int n_rich_strings = 0;
    for(int i = 0; i < strings->size(); i ++){
        auto tree = new EERTREE((*strings)[i]);
        int sub_palindromes_n = tree->subPalindromesN();
        if(sub_palindromes_n == (*strings)[i].size()){
            n_rich_strings ++;
        }
    }
    // number of rich strings can be printed for verification purposes
    // std::cout<< n_rich_strings * 2 << '\n';
    return;
}

static void bench(benchmark::State &state)
{
    int len = state.range(0);
    
    for (auto _ : state)
    {
        std::vector<std::string>* strings;
        strings = new std::vector<std::string>();
        AllStrings(strings, "0", len);
        palindromes(strings, len);
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
    ->Repetitions(ITER_N)
    ->ReportAggregatesOnly(true)
    ->Unit(benchmark::kSecond); // time in seconds

BENCHMARK_MAIN();