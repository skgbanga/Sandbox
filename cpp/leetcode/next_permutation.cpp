#include <iostream>
#include <vector>
#include <algorithm>
#include <limits>

void nextPermutation(std::vector<int>& nums)
{
   int sz = nums.size();
   if (sz == 0) // nothing to do
      return;

   // Look for the first inversion (a bigger number followed by a small number)
   auto iter = std::adjacent_find(nums.rbegin(), nums.rend(),
         [](auto lhs, auto rhs)
         {
            return lhs > rhs;
         });
   if (iter == nums.rend())
   {
      // we found no inversion, just sort the elements and return
      std::sort(nums.begin(), nums.end());
      return;
   }

   // we are interested in the inversion element
   ++iter;

   // look for the element which is the smallest largest element from *item
   int diff = std::numeric_limits<int>::max();
   auto it = nums.rbegin();
   auto the_one = nums.rbegin();
   for (; it != iter; ++it)
   {
      int local_diff = *it - *iter;
      if (local_diff <= 0)
         continue;

      if (local_diff < diff)
      {
         diff = local_diff;
         the_one = it;
      }
   }

   // swap the elements
   std::iter_swap(iter, the_one);

   // sort the vector iter + 1 -> nums.end()
   std::sort(nums.rbegin(), iter, std::greater<int>{});
}

int main(int argc, char *argv[])
{
   std::vector<int> v {1, 5, 1};
   nextPermutation(v);

   for (auto x : v)
      std::cout << x << ' ';
   std::cout << std::endl;

   return 0;
}
