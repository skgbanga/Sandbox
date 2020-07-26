/* Find the contiguous subarray within an array (containing at least one number) which has the largest sum. */

/* For example, given the array [-2,1,-3,4,-1,2,1,-5,4], */
/*     the contiguous subarray [4,-1,2,1] has the largest sum = 6. */

#include <vector>
#include <iostream>
#include <algorithm>
#include <iterator>

std::vector<int> maximal(const std::vector<int>& vec)
{
   if (vec.size() == 0)
      return {};

   // a table of (max, index)
   // - max: maximum sum of subarray ending at the index
   // - index: for that maximal sum, where does the subarray start
   std::vector<std::pair<int, int>> table (vec.size());

   // initialize the table
   table[0] = std::make_pair(vec[0], 0);

   for (int i = 1; i < vec.size(); ++i)
   {
      // get the previous entry in the table
      auto& prev = table[i-1];
      auto old_sum = prev.first;
      auto new_sum = old_sum + vec[i];
      if (new_sum > vec[i]) // last few elements helped us to get a better sum
         table[i] = std::make_pair(new_sum, prev.second);
      else
         // we are good being outselves only
         table[i] = std::make_pair(vec[i], i);
   }

   // we have filled our table fully
   auto max = std::max_element(table.begin(), table.end(),
         [](auto lhs, auto rhs) { return lhs.first < rhs.first; });
   auto elem = *max;

   int index = std::distance(table.begin(), max);
   auto start = vec.begin();
   std::advance(start, elem.second);
   auto end = vec.begin();
   std::advance(end, index + 1);

   return std::vector<int>(start, end);
}

int main(int argc, char *argv[])
{
   std::vector<int> v {-2, 1, -3, 4, -1, 2, 1, -5, 4};
   const auto& r = maximal(v);
   std::cout << "{";
   for (auto x : r)
      std::cout << x << ", ";
   std::cout << "}" << std::endl;
   return 0;
}
