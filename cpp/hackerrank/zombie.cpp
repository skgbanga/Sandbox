#include <iostream>
#include <vector>

// DFS from an index
void visit(int index,
           const std::vector<std::vector<int>>& matrix,
           std::vector<int>& visited)
{
   // pre-condition visited[index] = 1
   int sz = matrix.size();
   for (int j = 0; j < sz; ++j)
   {
      // if we are not neighbours, nothing to do
      if (matrix[index][j] == 0)
         continue;

      // if we are neighbours, but I have already visited you, nothing to do
      if (visited[j] != 0)
         continue;

      visited[j] = 1;
      visit(j, matrix, visited);
   }
}

int cluster(const std::vector<std::vector<int>>& matrix)
{
   // perform dfs on the adjacency matrix to calculate the number
   // of strongly connected compnents
   int num_components = 0;

   int sz = matrix.size();
   std::vector<int> visited (sz);
   for (int i = 0; i < sz; ++i)
   {
      // we have already visited this node
      if (visited[i] != 0)
         continue;

      visited[i] = 1;
      ++num_components;

      // Perform DFS
      visit(i, matrix, visited);
   }
   return num_components;
}

int main(int argc, char *argv[])
{
   // std::vector<std::vector<int>> matrix = {
   //    {1, 1, 0, 0},
   //    {1, 1, 1, 0},
   //    {0, 1, 1, 0},
   //    {0, 0, 0, 1}
   // };
   std::vector<std::vector<int>> matrix = {
      {1, 0, 0, 0, 0},
      {0, 1, 0, 0, 0},
      {0, 0, 1, 0, 0},
      {0, 0, 0, 1, 0},
      {0, 0, 0, 0, 1},
   };
   std::cout << cluster(matrix)  << '\n';
   return 0;
}

