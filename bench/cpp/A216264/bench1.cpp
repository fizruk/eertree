/*
* Google benchmark
* https://github.com/google/benchmark
* This implementation uses one tree for all strings.
* It utilizes some of the past computations, could be improved on.
*/
#include <benchmark/benchmark.h>
#include "../eertree1.h"

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

void testBasicFunctionalities(){
    // testing code
    EERTREE eertree;

    std::string s = "feddddehfa";

    // Insert the initial string into the EERTREE
    for (int i = 0; i < s.size(); i++)
    {
        eertree.insert(s, i);
    }

    // Print the initial palindromes
    std::cout << "Initial Palindromes:" << std::endl;
    eertree.printPalindromes(s);
    std::cout << std::endl;
    // Perform deletions and verify the updated palindromes after each deletion
    for (int i = s.size() -1 ; i > 0 ; i -- )
    {
        eertree.deleteLast(s, i);

        std::cout << "After Deletion " << i << ":" << std::endl;
        eertree.printPalindromes(s);
        std::cout << std::endl;
    }
    for (int i = 1; i < s.size(); i++)
    {
        std::cout<< "After insertion " << i << ": " << s[i] <<"\n";
        eertree.insert(s, i);
        eertree.printPalindromes(s);
        std::cout << std::endl;
    }
    int X;
    eertree.palindromes_n(s, X);
    std::cout<<"Number of unique palindromes is " << X << '\n';
    return;
}

void testModify(){
   EERTREE tree;

    std::string s = "dccccccd";

    // Insert the initial string into the EERTREE
    for (int i = 0; i < s.size(); i++)
    {
        tree.insert(s, i);
    }
    // Print the initial palindromes
    std::cout << "Initial Palindromes:" << std::endl;
    tree.printPalindromes(s);

    modifyEertree(tree, s, "dccccccf");
    std::cout << "After changing last character, palindromes:\n";
    tree.printPalindromes("dccccccf");

    modifyEertree(tree, "dccccccf", "dcccdfff");    
    std::cout << "After changing half the string, palindromes: \n";
    tree.printPalindromes("dcccdfff");

    modifyEertree(tree, "dccccccf", "daaaaaad");
    std::cout << "After changing almost all the string, palindromes: \n";
    tree.printPalindromes("daaaaaad");
     
    std::string str1 = "ababbaabaab", str2 = "ababbaababa";
    EERTREE tree2;
    for(int i = 0; i < str1.size(); i ++){
        tree2.insert(str1, i);
    }
    std::cout<<"initial palindromes\n";
    tree2.printPalindromes(str1);
    modifyEertree(tree2, str1, str2);
    std::cout<<"After modification\n";
    tree2.printPalindromes(str2);
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
    // Number of rich strings can be printed for verification purposes.
    // std::cout<< n_rich_strings * 2 << '\n';

    // Testing of needed functionalities
    // testBasicFunctionalities();
    // testModify();
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
