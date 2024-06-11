#define ALPHA_LOWERCASE
#define ONLINE_FREQ

#ifdef EERTREE_SEMI_PERS
#include "eertree_semi_pers.h"
#else
#include "eertree_direct.h"
#endif

#include <iostream>
#include <string>

void process_queries(int& q, std::string& queries)
{
    EERTREE eertree;
    std::string current_string = "";
    int current_ind = -1;
    for(char q_value : queries)
    {
        if (q_value != '-')
        {
            current_string += q_value;
            current_ind ++;
            eertree.insert(current_string, current_ind);
        }
        else
        {
            if (current_ind < 0) {
                continue;
            }
            eertree.deleteLast(current_string, current_ind);
            current_string.pop_back();
            current_ind --;
        }
        int NP = 0;
        eertree.all_palindromes_n(current_string, NP);
        #ifndef BENCH_RUN
        std::cout << NP << ' ';
        #endif
    }
}