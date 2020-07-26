#include <iostream>
#include <vector>
#include <algorithm>

class Solution {
   public:
      std::vector<std::vector<int>> combinationSum(const std::vector<int>& candidates, int target) const
      {
         std::vector<std::vector<std::vector<int>>> table (target + 1);

         // initialize
         table[0] = {{}};

         for (int idx = 1; idx <= target; ++idx)
         {
            auto& res = table[idx];
            for (std::size_t i = 0; i < candidates.size(); ++i)
            {
               int diff = idx - candidates[i];
               if (diff < 0)
                  continue;

               auto& diff_combs = table[diff];
               for (auto comb : diff_combs)
               {
                  comb.push_back(candidates[i]);
                  std::sort(comb.begin(), comb.end());
                  auto iter = std::find(res.begin(), res.end(), comb);
                  if (iter != res.end())
                     continue;

                  res.push_back(std::move(comb));
               }
            }
         }
         return table[target];
      }
};

Solution makeSol()
{
   return Solution{};
}

int main(int argc, char *argv[])
{
   auto res = makeSol().combinationSum({2, 3, 6, 7}, 7);
   std::for_each(res.begin(), res.end(), [](const std::vector<int>& val)
         {
            std::copy(val.begin(), val.end(), std::ostream_iterator<int>(std::cout, " "));
            std::cout << '\n';
         });
   return 0;
}
