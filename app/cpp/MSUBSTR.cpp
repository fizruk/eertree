/* You have t strings, for each string, get the length of the longest 
* sub-palindrome, and the number of sub-palindromes with such length.
* This problem made us introduce offline-frequency computation*/

#include "eertree.h"
#include <iostream>
#include <string>
#define f first 
#define s second 

std::pair<int, int> get_mubstr(EERTREE* tree, const std::string str) {
    std::string result;
    int size = tree->pointer;

    std::pair<int,int> res = {0,0}; // size, frequency; 
    for(int i = 3; i <= size; i++) {
        if(tree->tree[i].len > res.f) {
            res.f = tree->tree[i].len;
            res.s = tree->freq[i];
        }
        else
        if(tree->tree[i].len == res.f) {
            res.s += tree->freq[i];
        }
    }
    return res;
}

int main() {
    int t;
    std::string str;
    std::cin >> t;
    while (t--) {
        std::cin >> str;
        EERTREE eertree;
        for(int i = 0; i < str.size(); i++) {
            eertree.insert(str, i);
        }
        std::pair<int, int> res = get_mubstr(&eertree, str);
        std::cout << res.f << ' ' << res.s << '\n';
    }
}