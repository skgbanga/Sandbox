#include "Pipeline.h"
#include "IStage.h"

struct Stage1 : IStage
{
   int evaluate() override
   {
      return 0; // always return 0
   }
};

struct Stage2 : IStage
{
   int evaluate() override
   {
      // This function is returning following sequence [0, -1, 1, 0, -1, 1]
      // on being called
      static int cnt = 0;

      int ret = 0;

      if (cnt % 3 == 0)
         ret = 0;
      else if (cnt % 3 == 1)
         ret = -1;
      else
         ret = 1;

      ++cnt;
      return ret;
   };
};

int main(int argc, char *argv[])
{
   // test case 1 - sandwich stage2 between two stage1's
   // This should print [0 1 0 1 2 2] infinite times
   {
      Pipeline p;
      p.add(std::unique_ptr<IStage>(new Stage1()));
      p.add(std::unique_ptr<IStage>(new Stage2()));
      p.add(std::unique_ptr<IStage>(new Stage1()));

      p.run();
   }
   return 0;
}
