/* print a struct using boost hanna */

#include <iostream>
#include "StructPrinter.h"

#include <boost/hana/define_struct.hpp>

namespace hana = boost::hana;

struct Foo
{
   BOOST_HANA_DEFINE_STRUCT(Foo,
         (int, var1_),
         (double, var2_),
         (std::string, var3_)
         );
};

int main()
{
   Foo f { 10, 12.4, "hello world"};
   print_struct(f, std::cout);
}
