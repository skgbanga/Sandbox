#include <algorithm>
#include <iostream>
#include <iterator>
#include <vector>
#include <bitset>
#include <numeric>

template <typename T>
void print_vec(const std::vector<T>& vec)
{
   for (const auto& e : vec)
      std::cout << e << ' ';
   std::cout << std::endl;
}

void print_vec(const std::vector<std::pair<int, int>>& vec)
{
   for (const auto& e : vec)
      std::cout << "(" << e.first << "," << e.second << ")" << ' ';
   std::cout << '\n';
}

bool is_pow_2(int n)
{
   std::bitset<sizeof(n) * 8> bits(n);
   return bits.count() == 1;
}

bool is_not_pow_2(int n)
{
   return !is_pow_2(n);
}

int main(int argc, char *argv[])
{
   const auto vec = []
   {
      std::vector<int> vec (20);
      std::iota(vec.begin(), vec.end(), 0);
      return vec;
   }();

   // zip
   // zipWith
   // map
   // filter

   // zip - sort of: respects the size of first (begin, end)
   {
      std::vector<std::pair<int, int>> result {};
      std::transform(vec.begin(), vec.end(), vec.begin(), std::back_inserter(result),
            [](auto x, auto y) { return std::make_pair(x, y); });
      print_vec(result);
   }
   // zipWith
   {
      // std::tranform (two ranges, BinaryOperation)
   }
   // fitler -  haskell - `filter is_pow_2 vec`
   {
      auto result = vec;
      auto iter = std::remove_if(result.begin(), result.end(), is_not_pow_2);
      result.erase(iter, result.end());
      print_vec(result);
   }
   return 0;
}
