#include <iostream>
#include <string>
#include <vector>

void print(std::vector<std::vector<int>>& table)
{
   int lsize = table.size();
   int rsize = table[0].size();
   for (int j = 0; j < lsize; ++j)
   {
      for (int i = 0; i < rsize; ++i)
      {
         std::cout << table[j][i] << ' ';
      }
      std::cout << std::endl;
   }
}

int levenshtein(std::string lhs, std::string rhs)
{
   int lsize = lhs.size();
   int rsize = rhs.size();

   std::vector<std::vector<int>> table;
   table.resize(lsize);

   for (auto& x : table)
      x.resize(rsize);

   // base case, fill the corner rows
   for (int i = 0; i < rsize; ++i)
      table[0][i] = i;

   for (int j = 0; j < lsize; ++j)
      table[j][0] = j;

   for (int j = 1; j < lsize; ++j)
   {
      for (int i = 1; i < rsize; ++i)
      {
         int factor = 1;
         if (rhs[i] == lhs[j])
            factor = 0;
         int val1 = table[j - 1][i - 1] + factor;
         int val2 = table[j - 1][i] + 1;
         int val3 = table[j][i - 1] + 1;

         table[j][i] = std::min({val1, val2, val3});
      }
   }

   /* print(table); */
   return table[lsize - 1][rsize - 1];
}

int main(int argc, char *argv[])
{
   std::string lhs = "cassandra";
   std::string rhs = "morgan";
   auto dis = levenshtein(lhs, rhs);
   std::cout << dis << std::endl;
   return 0;
}
