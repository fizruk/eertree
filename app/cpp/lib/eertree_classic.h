/*
Classic eertree
    * Insertion only
    * Offline frequency computation -> if OFFLINE_FREQ is defined
    * Predefined Fixed size alphabet -> ALPHA_SIZE -> defined alphabet type
    * Predefined max number of sub-palindromes -> N
(Good for msubstr and rich strings)
*/
#include <iostream>
#include <string>
#include <cmath>
#ifdef ALPHA_LOWERCASE
    #define N 50005
    #define ALPHA_SIZE 26
    #define INIT_ALPHA 'a'
#elif defined(ALPHA_BINARY)
    #define N 50005
    #define ALPHA_SIZE 2
    #define INIT_ALPHA '0'
#else 
    #define N 10
    #define ALPHA_SIZE 2
    #define INIT_ALPHA '.'
#endif

struct Node
{
    int start;
    int len;

    int suffix;
    int edges[ALPHA_SIZE] = {};
};

class EERTREE
{
// private:
public:
    Node root1;
    Node root2;
    Node tree[N];
    #ifdef OFFLINE_FREQ
    int freq[N] = {};
    #endif
    int current;
    int pointer;
    int total_occ;

    EERTREE()
    {
        root1.len = -1;
        root1.suffix = 1;

        root2.len = 0;
        root2.suffix = 1;

        tree[1] = root1;
        tree[2] = root2;

        current = 1;
        pointer = 2;
        total_occ = 0;
    }

    void insert(const std::string s, int idx)
    {
        /* 
        * Search for Node X such that s[idx] X s[idx]
        * is maximum palindrome ending at position idx
        * iterate down the suffix link of current node to
        * find X 
        */
        int tmp = current;
        while (true)
        {
            int currentLen = tree[tmp].len;
            if (idx - currentLen >= 1 and s[idx] == s[idx - currentLen - 1])
                break;
            if(tmp == 0 && currentLen == 0)
                break;
            tmp = tree[tmp].suffix;
        }

        if (tree[tmp].edges[s[idx] - INIT_ALPHA] != 0)
        {
            current = tree[tmp].edges[s[idx] - INIT_ALPHA];
            #ifdef OFFLINE_FREQ
            freq[current] ++;
            #endif
            return;
        }
        // Creating new Node
        pointer += 1;
        // Making new Node as child of X with
        tree[tmp].edges[s[idx] - INIT_ALPHA] = pointer;
        tree[pointer].len = tree[tmp].len + 2;
        tree[pointer].start = idx - tree[pointer].len + 1;

        /* 
        * Setting the suffix edge for the newly created
        * Node tree[pointer]. Finding some String Y such that
        * s[idx] + Y + s[idx] is longest possible
        * palindromic suffix for newly created Node
        */
        tmp = tree[tmp].suffix;

        // making new Node as current Node
        current = pointer;
        #ifdef OFFLINE_FREQ
        freq[current] = 1;
        #endif
        if (tree[current].len == 1)
        {
            tree[current].suffix = 2;
            return;
        }
        while (true)
        {
            int currentLen = tree[tmp].len;
            if (idx - currentLen >= 1 and s[idx] == s[idx - currentLen - 1])
                break;
            tmp = tree[tmp].suffix;
        }
        /*
        * Now we have found string Y
        * linking current Nodes suffix link with s[idx]+Y+s[idx]
        */
        tree[current].suffix = tree[tmp].edges[s[idx] - INIT_ALPHA];
    }

    void unique_palindromes_n(const std::string& s, int& NP) {
        NP = pointer - 2;
    }

    void all_palindromes_n(const std::string& s, int& NP) {
        NP = total_occ;
    }

    #ifdef OFFLINE_FREQ
    void compute_offline_freq()
    {
        for (int i = pointer; i >= 3; i--)
        {
            freq[tree[i].suffix] += (freq[i]-1);
            total_occ += freq[i];
        }
    }
    #endif

};