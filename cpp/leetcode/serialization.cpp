#include <iostream>
#include <string>
#include <stack>

bool correct_stack(std::stack<std::string>& st)
{
   if (st.size() < 3)
      return false;

   auto t1 = st.top(); st.pop();
   auto t2 = st.top(); st.pop();
   auto t3 = st.top(); st.pop();

   if ((t1 == "#") && (t2 == "#") && (t3 != "#"))
   {
      st.push("#");
      return true;
   }
   else
   {
      // re-insert the elements
      st.push(t3);
      st.push(t2);
      st.push(t1);
   }

   return false;
}
bool isValidSerialization(std::string preorder)
{
   std::stack<std::string> st;

   const int sz = preorder.size();
   std::string str = "";
   for (int i = 0; i < sz; ++i)
   {
      if (preorder[i] == ',')
      {
         if (str.size() != 0)
         {
            st.push(str);
            str = ""; // reset the string
         }

         while (correct_stack(st));
         continue;
      }

      if (preorder[i] == '#')
      {
         st.push("#");
         continue;
      }

      str.push_back(preorder[i]);
   }

   if (str.size() != 0)
      st.push(str);

   // try to correct the stack after we are done
   while (correct_stack(st));

   std::cout << st.size() << '\n';

   return (st.size() == 1) && (st.top() == "#");
}

int main(int argc, char *argv[])
{
   std::cout << std::boolalpha << isValidSerialization("9,3,4,#,#,1,#,#,2,#,6,#,#") << std::endl;
   std::cout << std::boolalpha << isValidSerialization("1,#") << std::endl;
   std::cout << std::boolalpha << isValidSerialization("9,#,#,1") << std::endl;
   return 0;
}
