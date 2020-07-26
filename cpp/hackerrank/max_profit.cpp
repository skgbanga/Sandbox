#include <iostream>
#include <cmath>

int64_t max_profit(int64_t n)
{
   int64_t profit = 0;
   profit += std::floor(n / 2);
   profit += std::floor(n / 3);
   profit += std::floor(n / 4);
   profit = profit - n;
   return std::max(int64_t(0), profit);
}

int main(int argc, char *argv[])
{
   std::cout << max_profit(16) << '\n';
   std::cout << max_profit(7) << '\n';
   return 0;
}
