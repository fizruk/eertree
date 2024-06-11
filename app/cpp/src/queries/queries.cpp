#include "queries.h"

int main()
{
    int q;
    std::string queries;
    std::cin >> q >> queries;
    process_queries(q, queries);
    return 0;
}