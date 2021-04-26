/*
 * Taken and adopted from https://iq.opengenus.org/palindromic-tree-eertree/,
 * April 2021, for the needs of the comparative benchmarking.
 * 
 * C++ 11 code to construct an EERTREE. The method printAll
 * will print all subpalindromes stored in the tree.
 */

#include <iostream>
#include <vector>
#include <string>

struct Node
{
    int start, end;
    int len;

    Node *suffix;
    std::vector<Node *> labeled;

    Node()
    {
        start = end = len = -1;
        suffix = nullptr;
        labeled.assign(26, nullptr);
    }
};

class EERTREE
{
private:
    Node *root1;
    Node *root2;
    Node *current;

public:
    EERTREE()
    {
        root1 = new Node();
        root1->len = -1;
        root1->suffix = root1;

        root2 = new Node();
        root2->len = 0;
        root2->suffix = root1;
        current = root2;
    }

    int insert(const std::string &s, int pos)
    {
        Node *cur = current;
        int letter = s[pos] - 'a';

        while (true)
        {
            // loop is guranteed to break at root1
            if (pos - 1 - cur->len >= 0 && s[pos - 1 - cur->len] == s[pos])
                break;
            cur = cur->suffix;
        }

        Node *temp = new Node();
        temp->len = cur->len + 2;
        temp->end = pos;
        temp->start = pos - temp->len + 1;
        cur->labeled[letter] = temp;

        if (temp->len == 1)
        {
            // if cur is root1
            temp->suffix = root2;
            current = temp;
            return 0;
        }

        // find the suffix node

        while (true)
        {
            cur = cur->suffix;
            if ((pos - 1 - cur->len) >= 0 && s[pos - 1 - cur->len] == s[pos])
            {
                temp->suffix = cur->labeled[letter];
                break;
            }
        }
        current = temp;

        return 0;
    }

    void print(const std::string &s, Node *node)
    {
        if (node != root1 || node != root2)
        {
            for (int i = node->start; i <= node->end; ++i)
                std::cout << s[i];
            std::cout << '\n';
        }
        for (int i = 0; i < 26; ++i)
        {
            if (node->labeled[i] != nullptr)
            {
                print(s, node->labeled[i]);
            }
        }
    }

    void printAll(const std::string &s)
    {
        print(s, root1);
        print(s, root2);
    }

    void palindromes(const std::string &s, Node *node, std::string &result)
    {
        if (node != root1 || node != root2)
        {
            for (int i = node->start; i <= node->end; ++i)
                result += s[i];
            result += '\n';
        }
        for (int i = 0; i < 26; ++i)
        {
            if (node->labeled[i] != nullptr)
            {
                palindromes(s, node->labeled[i], result);
            }
        }
    }

    std::string palindromesAll(const std::string &s)
    {   
        std::string result = "";
        palindromes(s, root1, result);
        palindromes(s, root2, result);
        return result;
    }
};

/* Main needs to be commented out to be able to run it from Main.hs */
// int main(int argc, char *argv[])
// {
//     std::string s = argv[1];
//     EERTREE tree;
//     for (int i = 0; i < s.size(); ++i)
//         tree.insert(s, i);

//     tree.printAll(s);
//     // std::cout << tree.palindromesAll(s);
// }
