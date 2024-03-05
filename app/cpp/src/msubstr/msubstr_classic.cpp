/* You have t strings, for each string, get the length of the longest 
* sub-palindrome, and the number of sub-palindromes with such length.
* This problem made us introduce offline-frequency computation*/
#pragma GCC optimize("O3")
#pragma GCC optimize("unroll-loops")
#define ALPHA_LOWERCASE
#define OFFLINE_FREQ

#include "eertree_classic.h"
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

int main()
{
    int t;
    scanf("%d", &t);
    while (t--) {
        std::string str;
        std::cin >> str;
        std::pair<int, int> res;
        get_msubstr(str, res);
        printf("%d %d\n", res.fst, res.snd);
    }
}