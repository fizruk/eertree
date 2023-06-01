/*
 * Taken and adapted from https://www.geeksforgeeks.org/palindromic-tree-introduction-implementation/,
 * June 2021, for the needs of the comparative benchmarking.
 * 
 * C++ 11 code to construct an EERTREE.
 */
#include <iostream>
#include <string>

#define N 10000

struct Node
{
    int start, end;
    int len;

    int suffix;
    int edges[26] = {};
};

class EERTREE
{
private:
    Node root1;
    Node root2;
    Node tree[N];

    int current;
    int pointer;

public:
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
            tmp = tree[tmp].suffix;
        }

        /* 
        * Now we have found X
        * X = string at Node tmp
        * Check: if s[idx] X s[idx] already exists or not 
        */
        if (tree[tmp].edges[s[idx] - 'a'] != 0)
        {
            std::cout<<"okay i just don't even insert it\n";
            // s[idx] X s[idx] already exists in the tree
            current = tree[tmp].edges[s[idx] - 'a'];
            return;
        }

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
            /*
            * If new palindrome's length is 1,
            * make its suffix link to be null string
            */
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
        tree[current].suffix = tree[tmp].edges[s[idx] - 'a'];
    }

    void deleteLast(const std::string s, int idx)
    {
        // Get parent
        int tmp = current - 1;
        while (true)
        {
            int currentLen = tree[tmp].len;
            if (idx - currentLen >= 1 && s[idx] == s[idx - currentLen - 1])
                break;
            tmp = tree[tmp].suffix;
        }
        // Check if the edge doesn't exist already
        if (tree[tmp].edges[s[idx] - 'a'] == 0)
        {
            return;
        }
        // Remove edge
        tree[tmp].edges[s[idx] - 'a'] = 0;
        // Removing last node, pointing to earlier one
        pointer--;
        current = pointer;
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
    void palindromes_n(const std::string& s, int& NP) {
        std::set<std::string> uniquePalindromes; 
        for (int i = 3; i <= pointer; i++) {
            std::string palindrome = "";
            for (int j = tree[i].start; j <= tree[i].end; j++) {
                palindrome += s[j];
            }
            uniquePalindromes.insert(palindrome);
        }
        NP = uniquePalindromes.size();
    }
};
