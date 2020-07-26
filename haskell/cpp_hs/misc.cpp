#include <iostream>
#include <cmath>

int main(int argc, char *argv[])
{
   auto sum = 0u;
   auto cnt = 0u;
   while (true)
   {
      if (sum > 1000)
         break;
      ++cnt;
      sum += std::pow(cnt, 2);
   }
   std::cout << cnt << '\n';
   return 0;
}
