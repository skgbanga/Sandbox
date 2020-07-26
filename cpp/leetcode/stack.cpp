#include <vector>
#include <iostream>
#include <cmath>
#include <sstream>

static const int max = std::pow(2, 20) - 1;

bool sum(std::vector<int>& st)
{
   // there should be atleast two elements in the stack
   if (st.size() < 2)
      return false;

   int lhs = st.back();
   st.pop_back();
   int rhs = st.back();
   st.pop_back();

   // both the nums are 20 bits
   int s = lhs + rhs;
   if (s > max)
      return false;

   st.push_back(s);
   return true;
}

bool sub(std::vector<int>& st)
{
   // there should be atleast two elements in the stack
   if (st.size() < 2)
      return false;

   int lhs = st.back();
   st.pop_back();
   int rhs = st.back();
   st.pop_back();

   // both the nums are 20 bits
   int s = lhs - rhs;
   if (s < 0)
      return false;

   st.push_back(s);
   return true;
}

bool dup(std::vector<int>& st)
{
   if (st.size() == 0)
      return false;

   auto num = st.back();
   st.push_back(num);
   return true;
}

bool pop(std::vector<int>& st)
{
   if (st.size() == 0)
      return false;

   st.pop_back();
   return true;
}

int solution(std::string& s)
{
   int result = 0;

   std::vector<int> stack {};
   std::istringstream ss (s);

   while (ss.good())
   {
      std::string token = "";
      ss >> token;

      bool result = false;

      if (token == "DUP")
         result = dup(stack);

      else if (token == "POP")
         result = pop(stack);

      else if (token == "+")
         result = sum(stack);

      else if (token == "-")
         result = sub(stack);

      else
      {
         // only one possibility - this has to be number
         auto num = std::atoi(token.c_str());
         stack.push_back(num);
         result = true;
      }

      if (!result)
         return -1;
   }

   if (stack.size() == 0) // nothing in the stack at the end of the entire parsing!
      return -1;

   return stack.back();
}

int main(int argc, char *argv[])
{
   std::string s = "13 DUP 4 POP 5 DUP + DUP + -";
   std::cout << solution(s) << std::endl;
   return 0;
}
