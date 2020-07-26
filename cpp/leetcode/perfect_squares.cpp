#include <iostream>
#include <vector>
#include <cmath>
#include <numeric>

class Solution {
public:
    int numSquares(int n) const
    {
       // knapsack problem
       std::vector<int> knapsack (n + 1);

       auto is_square = [](int val)
       {
          auto x = std::sqrt(val);
          return x == std::floor(x);
       };

       // 0th index is not used
       for (int idx = 1; idx <= n; ++idx)
       {
          if (is_square(idx))
          {
             knapsack[idx] = 1;
             continue;
          }

          int opt = std::numeric_limits<int>::max();
          for (int j = 1; j <= idx/2; ++j)
             opt = std::min(opt, knapsack[j] + knapsack[idx - j]);

          knapsack[idx] = opt;
       }

       return knapsack[n];
    }
};

Solution makeSol()
{
   return Solution{};
}

int main(int argc, char *argv[])
{
   std::cout << makeSol().numSquares(9917) << '\n';
   return 0;
}
