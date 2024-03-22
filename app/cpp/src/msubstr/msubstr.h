/* You have t strings, for each string, get the length of the longest 
* sub-palindrome, and the number of sub-palindromes with such length.
* This problem made us introduce offline-frequency computation*/
#pragma GCC optimize("O3")
#pragma GCC optimize("unroll-loops")
#define ALPHA_LOWERCASE
#define OFFLINE_FREQ

#ifdef EERTREE_SEMI_PERS
#include "eertree_semi_pers.h" 
#elif defined(EERTREE_DIRECT)
#include "eertree_direct.h"
#else 
#include "eertree_classic.h"
#endif

#include <iostream>
#include <string>

#define fst first 
#define snd second 

std::pair<int, int> calc_mubstr(EERTREE* tree, const std::string str)
{
    std::string result;
    int size = tree->pointer;
    tree->compute_offline_freq();
    std::pair<int,int> res = {0,0}; // size, frequency; 
    for(int i = 3; i <= size; i++) {
        if(tree->tree[i].len > res.fst) {
            res.fst = tree->tree[i].len;
            res.snd = tree->freq[i];
        }
        else
        if(tree->tree[i].len == res.fst) {
            res.snd += tree->freq[i];
        }
    }
    return res;
}

void get_msubstr(std::string str, std::pair<int, int>& res)
{
    EERTREE eertree;
    for(int i = 0; i < str.size(); i++) {
        eertree.insert(str, i);
    }
    res = calc_mubstr(&eertree, str);

}