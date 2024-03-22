#ifndef SIMPLE_RICH
#include "rich.h"
#else 
#include "rich_simple.h"
#endif
#include <iostream>

int main()
{
    int len;
    std::cin >> len;
    compute_rich_strings_of(len);
    return 0;
}