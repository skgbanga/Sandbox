#include <memory>
#include <iostream>

struct Base
{
   ~Base() { std::cout << "Base::dtor" << '\n'; }
};

struct Derived : Base
{
   ~Derived() { std::cout << "Derived::dtor" << '\n'; }
};

int main()
{
   {
      /* auto spd = std::make_shared<Derived>(); */
      /* std::shared_ptr<Base> spb (static_cast<Base*>(spd.get())); */

      /* std::cout << spd.use_count() << '\n'; */
      /* std::cout << spb.use_count() << '\n'; */
   }

   {
      auto spd = std::make_shared<Derived>();
      std::shared_ptr<Base> spb (spd);

      std::cout << spd.use_count() << '\n';
      std::cout << spb.use_count() << '\n';
   }
}
