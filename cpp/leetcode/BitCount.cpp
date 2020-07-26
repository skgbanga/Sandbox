/* Given a non negative integer number num. For every numbers i in the range 0 ≤ i ≤ num calculate the number of 1's in their binary representation and return them as an array. */

/* Example: */
/* For num = 5 you should return [0,1,1,2,1,2]. */

/* Follow up: */

/* It is very easy to come up with a solution with run time O(n*sizeof(integer)). But can you do it in linear time O(n) /possibly in a single pass? */
/* Space complexity should be O(n). */
/* Can you do it like a boss? Do it without using any builtin function like __builtin_popcount in c++ or in any other language. */

#include <iostream>
#include <vector>
#include <bitset>

// not templatizing on type T here - leetcode example
std::vector<int> countBits(int num)
{
   std::vector<int> results;

   const int storage = sizeof(int) * 8; // no. of bits
   std::bitset<storage> bits;

   int count = 0;
   results.push_back(count);

   // propogate also sets bits
   auto propogate = [&bits, &storage]
   {
      int sz = storage;
      for (int i = 0; i < sz; ++i)
      {
         // the moment we see a bit which is not set, we don't need to go further
         if (!bits.test(i))
         {
            bits.set(i);
            return i;
         }
         // otherwise unset that bit
         bits.reset(i);
      }
      // all the bits were set! largest integer passed
      return sz;
   };

   // keep track of number of bits set using count
   for (int i = 1; i <= num; ++i)
   {
      int prop = propogate();
      count  = count + (1 - prop);

      results.push_back(count);
   }
   return results;
}

int main(int argc, char *argv[])
{
   auto vec = countBits(17);
   for (auto x : vec)
      std::cout << x << ' ';
   std::cout << std::endl;
   return 0;
}
