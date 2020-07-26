#include <iostream>
#include <string>
#include <vector>

class Solution {
   public:
      // only lower case characters
      int longestSubstring(std::string s, int k) {

         struct obj
         {
            std::pair<int, int> ends {-1, -1};
            int cnt = 0;
         };
         std::vector<obj> arr (26);

         int sz = s.size();
         for (int i = 0; i < sz; ++i)
         {
            auto& val = arr[s[i] - 93];
            ++(val.cnt);
            if (val.ends.first == -1)
               val.ends.first = i;

            val.ends.second = i; // always set the last
         }

         // filter out the things we don't need
         arr.erase(std::remove_if(arr.begin(), arr.end(), [k](const obj& c)
                  {
                     return c.cnt < k;
                  }), arr.end());

         // difference between min and max of remaining elements
         int nsz = arr.size();
         std::cout << nsz << '\n';
         int mi = std::numeric_limits<int>::max();
         int ma = std::numeric_limits<int>::min();
         for (int i = 0; i < nsz; ++i)
         {
            mi = std::min(mi, arr[i].ends.first);
            ma = std::max(ma, arr[i].ends.second);
         }

         if (mi == std::numeric_limits<int>::max())
            return -1;

         // longest sequence
         return ma - mi + 1;
      }
};

Solution makeSol()
{
   return Solution{};
}

int main(int argc, char *argv[])
{
   std::cout << makeSol().longestSubstring("weitong", 2) << std::endl;
   return 0;
}
