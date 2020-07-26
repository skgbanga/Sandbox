#include <vector>
#include <string>
#include <iostream>


template <typename T>
void print_vec(const std::vector<T>& vec)
{
   for (auto& x : vec)
      std::cout << x << ' ';

   std::cout << std::endl;
}

#include <algorithm>

struct tree
{
  int x;
  tree * l;
  tree * r;
};

std::vector<int> unique(tree* T)
{
   if (!T) // if nullptr, no unique value
      return {};

   // get number of unique values in the children
   auto left  = unique(T->l); // copies
   auto right = unique(T->r);

   // check to see if the root is there in them
   auto value = T->x;
   auto lamda = [value](std::vector<int>& vec)
   {
      auto iter_vec = std::find(vec.cbegin(), vec.cend(), value);
      if (iter_vec == vec.end())
         vec.push_back(value);
   };

   lamda(left);
   lamda(right);

   if (left.size() > right.size())
      return left;
   else
      return right;
}

std::vector<std::vector<int>> unique_paths(tree *T)
{
   // base case 1
   if (!T)
      return {{}};

   // base case 2 (needed)
   if (T->l && T->r)
      return {{T->x}};

   auto left = unique_paths(T->l);
   auto right = unique_paths(T->r);

   // check where is root in the unique paths of both left and right
   auto val = T->x;
   auto lamda = [val](std::vector<int>& vec)
   {
      auto fnd = std::find(vec.begin(), vec.end(), val);
      if (fnd == vec.end())
      {
         vec.push_back(val);
      }
      else
      {
         // need to truncate this vector till fnd
         std::advance(fnd, 1);
         vec.erase(vec.begin(), fnd); // removes all elements from begin and fnd (fnd included)
         vec.push_back(val);
      }
   };

   for (auto& vec : left)
      lamda(vec);

   for (auto& vec : right)
      lamda(vec);

   // concatenate both left and right
   std::vector<std::vector<int>> vec;
   return vec;
}

int solution(tree * T)
{
   const auto& u = unique(T);
   return u.size();
}

int main(int argc, char *argv[])
{
   return 0;
}


