/**
Direct links tree without memory persistance:
    * Online computation of frequency.
    * Insert and delete last.
    * Total/Unique number of palindromes.
    * Uses direct links
    * Predefined fixed alphabet size -> 26
    * Predefined max number of subpalindromes -> 30k
**/

#include <iostream>
#include <string>
#include <cmath>
#ifdef ALPHA_LOWERCASE
    #define N 30005
    #define ALPHA_SIZE 26
    #define INIT_ALPHA 'a'
#elif defined(ALPHA_BINARY)
    #define N 30005
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
    int directLinks[ALPHA_SIZE] = {};
    int edges[ALPHA_SIZE] = {};
};

class EERTREE
{
// private:
public:
    Node root1;
    Node root2;
    Node tree[N];
    bool addedNewNode[N];
    int myPriorTmp[N];
    int myPriorCurrent[N];
    #if defined(OFFLINE_FREQ) || defined(ONLINE_FREQ)
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
        for(int i = 0; i < ALPHA_SIZE; i++){
            tree[1].directLinks[i] = tree[2].directLinks[i] = 1;
        }
    }

    #ifdef ONLINE_FREQ
    void add_occ_current(int id)
    {
        if(id < 3)
            return;
        total_occ ++;
        freq[id] ++;
        add_occ_current(tree[id].suffix);
    }

    void minus_occ_current(int id)
    {
        if(id < 3)
            return;
        total_occ --;
        freq[id] --;
        minus_occ_current(tree[id].suffix);
    }
    #endif

    void insert(const std::string s, int idx)
    {
        /* 
        * Search for Node X such that s[idx] X s[idx]
        * is maximum palindrome ending at position idx
        * iterate down the suffix link of current node to
        * find X 
        */
        addedNewNode[idx] = false;
        int tmp = current;
        if (!(idx - tree[tmp].len >= 1 and s[idx] == s[idx - tree[tmp].len - 1]))
        {
            tmp = tree[tmp].directLinks[s[idx] -INIT_ALPHA];
        }
        // store the initial state of tmp and current for future deletion purposes.
        myPriorTmp[idx] = tmp;
        myPriorCurrent[idx] = current;

        if (tree[tmp].edges[s[idx] - INIT_ALPHA] != 0)
        {
            current = tree[tmp].edges[s[idx] - INIT_ALPHA];
            #ifdef ONLINE_FREQ
            add_occ_current(current);
            #elif defined(OFFLINE_FREQ)
            freq[current]++;
            #endif
            return;
        }
        addedNewNode[idx] = true;
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
        if (tree[current].len == 1)
        {
            tree[current].suffix = 2;
            #ifdef ONLINE_FREQ
            add_occ_current(current);
            #elif defined(OFFLINE_FREQ)
            freq[current] = 1;
            #endif
            std::copy(tree[tree[current].suffix].directLinks, 
                tree[tree[current].suffix].directLinks + ALPHA_SIZE, tree[current].directLinks);
            tree[current].directLinks[s[idx] - INIT_ALPHA] = 2;
            return;
        }

        if (!(idx - tree[tmp].len >= 1 and s[idx] == s[idx - tree[tmp].len - 1]))
        {
            tmp = tree[tmp].directLinks[s[idx] -INIT_ALPHA];
        }
        /*
        * Now we have found string Y
        * linking current Nodes suffix link with s[idx]+Y+s[idx]
        */
        tree[current].suffix = tree[tmp].edges[s[idx] - INIT_ALPHA];
        tmp = tree[current].suffix;
        #ifdef ONLINE_FREQ
        add_occ_current(current);
        #elif defined(OFFLINE_FREQ)
        freq[current]++;
        #endif
                std::copy(tree[tree[current].suffix].directLinks, 
            tree[tree[current].suffix].directLinks + ALPHA_SIZE, tree[current].directLinks);
        tree[current].directLinks[s[idx - tree[tmp].len] - INIT_ALPHA] = tree[current].suffix;
    }

    void deleteLast(const std::string s, int idx)
    {
        // No deletion of entire node is necessary
        if(!addedNewNode[idx]){
            #ifdef ONLINE_FREQ
            minus_occ_current(current);
            #elif defined(OFFLINE_FREQ)
            freq[current]--;
            #endif
            current = myPriorCurrent[idx];
            return;
        }

        // Deleting latest node
        #ifdef ONLINE_FREQ
        minus_occ_current(pointer);
        #elif defined(OFFLINE_FREQ)
        freq[pointer]--;
        #endif
        int tmp = myPriorTmp[idx];
        tree[tmp].edges[s[idx] - INIT_ALPHA] = 0;
        pointer -= 1;
        current = myPriorCurrent[idx];
        addedNewNode[idx] = false;
    }
    
    void resetFromNode(int index, std::string preDeletion)
    {   
        for (int i = preDeletion.size() - 1; i >= index; i--){
            this->deleteLast(preDeletion, i);
        }
    }

    void palindromes(const std::string &s, std::string &result)
    {
        for (int i = 3; i <= pointer; i++)
        {
            for (int j = tree[i].start; j < tree[i].start + tree[i].len; j++)
            {
                result += s[j];
            }
            result += '\n';
        }
    }
    
    #if defined(ONLINE_FREQ) || defined(OFFLINE_FREQ)
    void palindromes_freq(const std::string &s, std::string &result) {
        for (int i = 3; i <= pointer; i++)
        {
            for (int j = tree[i].start; j < tree[i].start + tree[i].len; j++)
            {
                result += s[j];
            }
            result += ' ';
            result += std::to_string(freq[i]);
            result += '\n';
        }
    }

    inline void all_palindromes_n(const std::string& s, int& NP) {
        NP = total_occ;
    }
    #endif

    inline void unique_palindromes_n(const std::string& s, int& NP) {
        NP = pointer - 2;
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
