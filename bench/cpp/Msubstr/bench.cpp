/*
* Google benchmark
* https://github.com/google/benchmark
*/
#include <benchmark/benchmark.h>
#include "../eertree1.h"
#include <iostream>
#include <ctime>
#include <unistd.h>

#define BENCH_N 500
#define ITER_N 3

static std::string randomString(const int len);

static void msubstr(const std::string str);

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

static std::string randomPalindrome(int len){
    std::string half_palindrome = randomString(len/2);
    std::string palindrome = half_palindrome;
    if(len %2)
    {
        half_palindrome += 'a';
    }
    std::reverse(half_palindrome.begin(), half_palindrome.end());
    palindrome += half_palindrome;
    return palindrome;
}

static void msubstr(const std::string str) {

    EERTREE* tree = new EERTREE();
    for (int i = 0; i < str.size(); i++)
    {
        tree->insert(str, i);
    }

    std::string result;
    int size = tree->pointer;
    std::pair<int,int> res = {0,0}; // size, frequency; 
    for(int i = 3; i <= size; i++) {
        if(tree->tree[i].len > res.first)
        {
            res.first = tree->tree[i].len;
            res.second = tree->freq[i];
        }
        else
        if(tree->tree[i].len == res.first)
        {
            res.second += tree->freq[i];
        }
    }
    return;
}

static void bench(benchmark::State &state)
{
    int len = state.range(0);


    for (auto _ : state)
    {
        std::string s = randomPalindrome(len);
        msubstr(s);
    }
}

BENCHMARK(bench)
    // String lengths
    ->Arg(BENCH_N)
    ->Arg(2 * BENCH_N)
    ->Arg(4 * BENCH_N)
    ->Arg(6 * BENCH_N)
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
