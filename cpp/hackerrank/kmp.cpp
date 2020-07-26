#include <vector>
#include <iostream>
#include <cassert>


// based on the pseudo code from wiki
std::vector<int> get_pos_table(const std::string& needle)
{
   int sz = needle.size();
   assert (sz >= 2);

   std::vector<int> table (sz);

   int pos = 2; // how far we have parsed into our needle
   int cnd = 0; // last index of the largest prefix

   // fill the table with first few values
   table[0] = -1; // init condition
   table[1] = 0;

   while (pos < sz)
   {
      if (needle[pos - 1] == needle[cnd])
      {
         table[pos] = cnd + 1;
         ++cnd;
         ++pos;
      }
      else
      {
         if (cnd > 0)
         {
            // get the best prefix match for cnd, and try to match it with pos
            cnd = table[cnd];
         }
         else
         {
            assert(cnd == 0);
            table[pos] = 0;
            ++pos;
         }
      }
   }

   return table;
}

int kmp_substr(const std::string& needle, const std::string& haystack)
{
   // variable m to go over haystack - denotes the start of the match with needle
   // variable i to go over needle
   int m = 0;
   int i = 0;

   // get the prefix table for the needle first
   auto table = get_pos_table(needle);

   int hsz = haystack.size();
   int nsz = needle.size();
   while (m + i < hsz)
   {
      if (haystack[m + i] == needle[i])
      {
         ++i;
         if (i == nsz)
            return m;
      }
      else
      {
         if (table[i] == -1) // can only happen on the first index
         {
            ++m;
            assert(i == 0);
         }
         else
         {
            m = m + i - table[i];
            i = table[i];
         }
      }
   }

   // if we are here implies we were not able find needle in the haystack
   return -1;
}

void print_vec(const std::vector<int>& vec)
{
   for (const auto& v : vec)
      std::cout << v << " ";
   std::cout << std::endl;
}

int main()
{
   std::string needle = "abcdabd";
   std::string haystack = "abc abcdab abcdabcdabde";

   auto idx = kmp_substr(needle, haystack);
   std::cout << idx << '\n';
   return 0;
}
