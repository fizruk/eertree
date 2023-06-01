/*
* Google benchmark
* https://github.com/google/benchmark
* This implementation does not use one tree
* It constructs a new tree for every new string to check if it's a rich string
*/
#include <benchmark/benchmark.h>
#include "eertree2.h"

#include <iostream>
#include <ctime>
#include <unistd.h>

#define ITER_N 1

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
    int ans = 0;
    for(int i = 0; i < strings->size(); i ++){
        auto tree = new EERTREE((*strings)[i]);
        int sub_palindromes_n = tree->subPalindromesN();
        if(sub_palindromes_n == (*strings)[i].size()){
            ans ++;
        }
    }
    std::cout<< ans * 2 << '\n';
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
    ->Arg(1)
    ->Arg(2)
    ->Arg(3)
    ->Arg(4)
    ->Arg(5)
    ->Arg(6)
    ->Arg(7)
    ->Arg(8)
    ->Arg(9)
    ->Arg(10)
    ->Arg(11)
    ->Arg(12)
    ->Arg(13)
    ->Arg(14)
    ->Arg(15)
    ->Arg(16)
    ->Arg(17)
    ->Arg(18)
    ->Arg(19)
    ->Arg(20)
    ->Iterations(ITER_N)        // # of iterations
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
