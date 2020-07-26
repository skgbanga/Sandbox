#include <iostream>
#include <string>
#include <vector>

std::string longest_palindrom_substring(std::string input)
{
   const std::size_t N = input.size();
   // construct a 2D array on which dp can be performed
   std::vector<std::vector<bool>> results;

   // initialize the 2D array to falses'
   // array size: N * N
   results.resize(N);
   for (auto& x : results)
      x.resize(N, false);

   for (int i = N - 1; i >= 0; --i)
   {
      for (int j = N - 1; j > i; --j)
      {
         if (j == i + 1) // one element is always a palindrome
            results[i][j] = true;

         if (results[i + 1][j - 1])
         {
            if (input[i] == input[j])
               results[i][j] = true;
         }
      }
   }

   for (int i = N - 1; i >= 0; --i)
   {
      for (int j = N - 1; j > i; --j)
      {
         if (results[i][j])
            std::cout << input.substr(i, j - i + 1);
      }
   }
   return "";
}

int main(int argc, char *argv[])
{
   std::string str = "forgeeksskeegfor";
   std::cout << longest_palindrom_substring(str) << std::endl;
   return 0;
}

