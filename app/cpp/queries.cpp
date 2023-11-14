#include "eertree.h"
#include <iostream>
#include <string>

int main() {
    EERTREE eertree;
    int q;
    char q_value;
    std::cin >> q;
 
    std::string current_string = "";
    int current_ind = -1;
    while (q--) {
        std::cin >> q_value;
        if (q_value != '-') {
            current_string += q_value;
            current_ind ++;
            eertree.insert(current_string, current_ind);
        }
        else {
            if (current_ind < 0) {
                continue;
            }
            eertree.deleteLast(current_string, current_ind);
            current_string.pop_back();
            current_ind --;
        }
        int NP = 0;
        eertree.all_palindromes_n(current_string, NP);
        std::cout << NP << ' ';
    }
    return 0;
}