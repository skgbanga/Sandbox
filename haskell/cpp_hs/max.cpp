#include <iostream>
#include <typeinfo>
#include <cxxabi.h>

auto print_name = [](auto func)
{
   auto status = 0;
   const auto& type = typeid(func);
   auto* realname = abi::__cxa_demangle(type.name(), 0, 0, &status);
   std::cout << realname << '\n';
   free(realname);
};

int g_max(int a, int b)
{
   return (a > b) ? a : b;
}

int g_max100(int b)
{
   return g_max(b, 100);
}

int main(int argc, char *argv[])
{
   auto max100 = [](int x)
   {
      return g_max(x, 100);
   };

   print_name(g_max);
   print_name(g_max100);

   print_name(print_name);
   print_name(max100);

   return 0;
}
