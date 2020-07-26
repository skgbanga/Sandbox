#include <iostream>
#include <algorithm>
#include <typeinfo>
#include <functional>

template <typename T>
class TypeDeducor;

struct my_int
{
   int my;
};

my_int operator+(const my_int& lhs, const my_int& rhs)
{
   return { lhs.my + rhs.my };
}

int main(int argc, char *argv[])
{
   auto num = std::max(10, 20);
   /* TypeDeducor<decltype(num)> td; */
   auto m1 = my_int {10};
   auto m2 = my_int {12};

   auto val = std::plus<>{}(m1, m2);
   std::cout << val.my << '\n';
   return 0;
}
