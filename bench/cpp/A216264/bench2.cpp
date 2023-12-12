/*
* Google benchmark
* https://github.com/google/benchmark
* This implementation uses one tree for all strings.
* It utilizes some of the past computations, could be improved on.
*/
#include <benchmark/benchmark.h>
#include "../eertree2.h"

#include <iostream>
#include <ctime>
#include <unistd.h>

#define BENCH_N 2
#define ITER_N 5

static void palindromes();

// Create a vector of all possible strings of size len.
static void AllStrings(std::vector<std::string>* strings, std::string str, const int len){
    if(str.size() == len){
        strings->push_back(str);
        return;
    }
    AllStrings(strings, str + "a", len);
    AllStrings(strings, str + "b", len);
}

// Modify the tree created for the past string to suit the current string.
static void modifyEertree(EERTREE& tree, std::string past, std::string current){
    // std::cout<<"modify: "<< past << ' ' << current << '\n';
    int n = 0;
    for(int i = 0; i < past.size(); i++){
        if(past[i] == current[i]){
            n++;
        }
        else{
            break;
        }
    }
    tree.resetFromNode(n, past);
    for(int i = n; i < current.size(); i++){
        tree.insert(current, i);
    }
    return;
}

/*
   -- Observation: there is exactly the same number of rich strings
   -- that start with 0 as there are those starting with 1.
   -- That is why we can do half work (or 1/(alphabet size) in general)
   -- and count rich strings faster.
*/
static void palindromes(std::vector<std::string>* strings, int len)
{
    // Calculating number of rich strings of size n
    EERTREE tree;
    int n_rich_strings = 0;
    /*
        Check if the first string in the vector of all possible
        strings of size n is a rich string.
    */
    for(int i = 0; i < (*strings)[0].size(); i ++){
        tree.insert((*strings)[0], i);
    }
    int x = 0;
    tree.palindromes_n((*strings)[0], x);
    if(x == len){
        n_rich_strings ++;
    }
    /*
        Modify the current tree for each of the potential string
        and check if it's a rich string.
    */ 
    for (int i = 1; i < strings->size(); ++i){
        modifyEertree(tree, (*strings)[i-1], (*strings)[i]);
        x = 0;
        tree.palindromes_n((*strings)[i], x);
        // Check if it's a rich string.
        if(x == len){
            n_rich_strings ++;
        }
    }
    return;
}

static void bench(benchmark::State &state)
{
    int len = state.range(0);
    std::vector<std::string>* strings;
    strings = new std::vector<std::string>();
    
    for (auto _ : state)
    {
        AllStrings(strings, "a", len);
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
    ->Arg(11 * BENCH_N)
    ->Repetitions(ITER_N)
    ->ReportAggregatesOnly(true)
    ->Unit(benchmark::kSecond); // time in seconds

BENCHMARK_MAIN();
