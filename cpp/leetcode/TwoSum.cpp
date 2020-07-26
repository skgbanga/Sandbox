/* ven an array of integers that is already sorted in ascending order, find two numbers such that they add up to a specific target number. */

/* The function twoSum should return indices of the two numbers such that they add up to the target, where index1 must be less than index2. Please note that your returned answers (both index1 and index2) are not zero-based. */

/* You may assume that each input would have exactly one solution. */

/* Input: numbers={2, 7, 11, 15}, target=9 */
/* Output: index1=1, index2=2 */

#include <unordered_map>
#include <vector>
#include <algorithm>
#include <iostream>
#include <cassert>

// O(n) solution with with O(n) space
std::pair<int, int> twoSum_hash(const std::vector<int>& numbers, int target)
{
   // stick all the numbers in a hash map
   using map_t = std::unordered_map<int, bool>;
   map_t omap;

   for (auto x : numbers)
   {
      omap[x] = true;
   }

   // go through the map again, and see whether we have corresponding number
   int num1 = 0; int num2 = 0;
   for (auto x : omap)
   {
      auto num = x.first;
      auto rnum = target - num;
      if (omap.find(rnum) == omap.end())
         continue;

      // we have found our numbers
      num1 = std::min(num, rnum);
      num2 = std::max(num, rnum);
   }

   // let's find the indices of num1 and num2
   int idx1 = 0; int idx2 = 0;
   for (int i = 0; i < numbers.size(); ++i)
   {
      if (numbers[i] == num1)
         idx1 = i;
      if (numbers[i] == num2)
         idx2 = i;
   }

   return {idx1 + 1, idx2 + 1};
}


// O(nlogn) solution based on binary search with O(1) space
// most likely faster in pratice
std::pair<int, int> twoSum_binary(const std::vector<int>& numbers, int target)
{
   assert(std::is_sorted(numbers.begin(), numbers.end()));
   for (int i = 0; i < numbers.size(); ++i)
   {
      auto num = numbers[i];
      auto tnum = target - num;

      // binary search for tnum in numbers
      auto result = std::binary_search(numbers.begin(), numbers.end(), tnum);
      if (result)
      {
         // we have found our numbers!
         // scan for tnum
         for (int j = i; j < numbers.size(); ++j) // has to be after i
         {
            if (numbers[j] == tnum)
               return {i + 1, j + 1};
         }
      }
   }

   // it is guarrantted that there is atleast one number
   return {0, 0};
}

// assuming all numbers are positive
int main(int argc, char *argv[])
{
   const int N = 10;
   std::vector<int> numbers { 5, 3, 56, 34, 2, 12, 24, 12, 29, 10 };
   std::sort(numbers.begin(), numbers.end());
   for (auto x : numbers)
      std::cout << x << " ";
   std::cout << std::endl;
   int target = 46;

   auto hindices = twoSum_hash(numbers, target);
   auto bindices = twoSum_binary(numbers, target);

   // the difference in answers is because of presence of values equal to 12
   std::cout << "hash indices " << hindices.first << " " << hindices.second << std::endl;
   std::cout << "binary indices " << bindices.first << " " << bindices.second << std::endl;

   return 0;
}
