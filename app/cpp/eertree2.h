/**
This version has:
    * Offline computation of frequency.
    * Insert and delete last.
    * Total number of palindromes.
    * Unique number of palindromes.
**/
#include <iostream>
#include <string>
#include <cmath>
#define N 30005

struct Node
{
    int start, end;
    int len;

    int suffix;
    int edges[26] = {};
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
    int freq[N] = {};
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
        addedNewNode[idx] = false;
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
        // store the initial state of tmp and current for future deletion purposes.
        myPriorTmp[idx] = tmp;
        myPriorCurrent[idx] = current;

        if (tree[tmp].edges[s[idx] - 'a'] != 0)
        {
            current = tree[tmp].edges[s[idx] - 'a'];
            freq[current] ++;
            return;
        }
        addedNewNode[idx] = true;
        // Creating new Node
        pointer += 1;
        // Making new Node as child of X with
        tree[tmp].edges[s[idx] - 'a'] = pointer;
        tree[pointer].len = tree[tmp].len + 2;
        tree[pointer].end = idx;
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
            freq[current] ++;
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
        tree[current].suffix = tree[tmp].edges[s[idx] - 'a'];
        freq[current] ++;
    }

    void deleteLast(const std::string s, int idx)
    {
        // No deletion is necessary
        if(!addedNewNode[idx]){
            freq[current] --;
            current = myPriorCurrent[idx];
            return;
        }

        // Deleting latest node
        freq[pointer]++;
        int tmp = myPriorTmp[idx];
        tree[tmp].edges[s[idx] - 'a'] = 0;
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

    void printPalindromes(const std::string &s)
    {
        for (int i = 3; i <= pointer; i++)
        {
            for (int j = tree[i].start; j <= tree[i].end; j++)
            {
                std::cout << s[j];
            }
            std::cout << std::endl;
        }
    }

    void palindromes(const std::string &s, std::string &result)
    {
        for (int i = 3; i <= pointer; i++)
        {
            for (int j = tree[i].start; j <= tree[i].end; j++)
            {
                result += s[j];
            }
            result += '\n';
        }
    }
    
    void palindromes_freq(const std::string &s, std::string &result) {
        for (int i = 3; i <= pointer; i++)
        {
            for (int j = tree[i].start; j <= tree[i].end; j++)
            {
                result += s[j];
            }
            result += ' ';
            result += std::to_string(freq[i]);
            result += '\n';
        }
    }
    
    void compute_freq()
    {
        for (int i = pointer; i >= 3; i--)
        {
            freq[tree[i].suffix] += (freq[i]-1);
            total_occ += freq[i];
        }
    }

    void palindromes_n(const std::string& s, int& NP) {
        NP = pointer - 2;
    }

    void all_palindromes_n(const std::string& s, int& NP) {
        NP = total_occ;
    }

};