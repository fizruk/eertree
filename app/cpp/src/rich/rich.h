#define ALPHA_BINARY
#ifdef EERTREE_SEMI_PERS
#include "eertree_semi_pers.h"
#else
#include "eertree_direct.h"
#endif
#include <iostream>
#include <ctime>
#include <unistd.h>
#include <vector>


// Create a vector of all possible strings of size len.
void AllStrings(std::vector<std::string>* strings, std::string str, const int len)
{
    if(str.size() == len){
        strings->push_back(str);
        return;
    }
    AllStrings(strings, str + "0", len);
    AllStrings(strings, str + "1", len);
}

// Modify the tree created for the past string to suit the current string.
void modifyEertree(EERTREE& tree, std::string past, std::string current)
{
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
void palindromes(std::vector<std::string>* strings, int len)
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
    tree.unique_palindromes_n((*strings)[0], x);
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
        tree.unique_palindromes_n((*strings)[i], x);
        // Check if it's a rich string.
        if(x == len){
            n_rich_strings ++;
        }
    }
    // Number of rich strings can be printed for verification purposes.
    #ifndef BENCH_RUN
    std::cout<< n_rich_strings * 2 << '\n';
    #endif
    return;
}

void compute_rich_strings_of(int len)
{
    std::vector<std::string>* strings;
    strings = new std::vector<std::string>();
    AllStrings(strings, "0", len);
    palindromes(strings, len);
}
