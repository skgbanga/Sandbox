#include <vector>
#include <iostream>
#include <algorithm>
#include <iterator>

std::vector<std::vector<int>> permute(std::vector<int>& vec)
{
   if (vec.size() == 0)
      return {};

   // base case
   if (vec.size() == 1)
      return {{vec[0]}};

   // pop the last element
   auto elem = vec.back();
   vec.pop_back();
   auto base = permute(vec); // recursive call - vector of vecs

   std::vector<std::vector<int>> fin {};
   for (auto& inter : base)
   {
      int sz = inter.size();
      for (int i = 0; i <= sz; ++i)
      {
         auto inter_copy = inter;
         inter_copy.insert(inter_copy.begin() + i, elem);
         fin.push_back(std::move(inter_copy));
      }
   }
   return fin;
}

int main(int argc, char *argv[])
{
   std::vector<int> inp = {1, 2, 3, 5};
   auto result = permute(inp);

   for (auto& indiv : result)
   {
      for (auto x : indiv)
         std::cout << x << ' ';
      std::cout << '\n';
   }
   return 0;
}
