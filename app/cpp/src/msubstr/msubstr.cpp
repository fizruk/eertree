#include "msubstr.h"

int main()
{
    int t;
    scanf("%d", &t);
    std::string str;
    while(t--)
    {
        std::cin >> str; 
        std::pair<int, int> res;
        get_msubstr(str, res);
        printf("%d %d\n", res.fst, res.snd);
    }
}