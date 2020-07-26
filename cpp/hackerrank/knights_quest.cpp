#include <iostream>
#include <set>

struct position_t
{
   int x = 0;
   int y = 0;

   friend bool operator==(const position_t& lhs, const position_t& rhs)
   {
      return lhs.x == rhs.x && lhs.y == rhs.y;
   }
   friend bool operator<(const position_t& lhs, const position_t& rhs)
   {
      return std::tie(lhs.x, lhs.y) < std::tie(rhs.x, rhs.y);
   }
};

std::set<position_t> possible_moves(position_t init)
{
   std::set<position_t> results {};
   auto checker = [&init, &results](int xoff, int yoff)
   {
      int xfin = init.x + xoff;
      int yfin = init.y + yoff;
      if (xfin > 7 || xfin < 0)
         return;
      if (yfin > 7 || yfin < 0)
         return;

      // add to the result is no already in there
      results.insert(position_t{xfin, yfin});
   };
   checker(1, 2);
   checker(2, 1);
   checker(2, -1);
   checker(1, -2);
   checker(-1, -2);
   checker(-2, -1);
   checker(-2, 1);
   checker(-1, 2);

   return results;
};

bool reachable(position_t init, position_t final)
{
   if (init == final)
      return true;

   std::set<position_t> positions {init};
   for (int i = 0; i < 3; ++i)
   {
      std::set<position_t> newpos {};
      for (auto p : positions)
      {
         auto nps = possible_moves(p);
         auto iter = std::find_if(nps.begin(), nps.end(), [&final](position_t p) { return p == final; });
         if (iter != nps.end())
            return true;

         newpos.insert(nps.begin(), nps.end());
      }
      positions = newpos;
   }

   return false;
};

int main(int argc, char *argv[])
{
   bool result = reachable({1, 1}, {7, 7});
   std::cout << std::boolalpha << result << '\n';
   return 0;
}
