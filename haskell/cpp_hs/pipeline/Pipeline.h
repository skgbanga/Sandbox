#pragma once

#include "IStage.h"

#include <cassert>
#include <iostream>
#include <memory>
#include <vector>

struct Pipeline
{
   void run()
   {
      // call loop 90 times
      for (int i = 0; i < 90; ++i)
         evaluate();
   }

   void evaluate()
   {
      evaluate_stages(m_stages, 0);
   };

   void add(std::unique_ptr<IStage> ps)
   {
      m_stages.push_back(std::move(ps));
   }

   private:
   // stages not marked 'const' as their evaluate function need to update their internal states
   void evaluate_stages(std::vector<std::unique_ptr<IStage>>& stages, std::size_t idx)
   {
      // end case of recursion
      if (idx == stages.size())
         return;

      auto& st = stages[idx];
      const auto result = st->evaluate();
      std::cout << idx << " ";
      if (result > 0)
      {
         evaluate_stages(stages, idx + 1);
         evaluate_stages(stages, idx);
      }
      else if (result == 0)
      {
         evaluate_stages(stages, idx + 1);
      }
      // if negative, we are done
   }

   std::vector<std::unique_ptr<IStage>> m_stages;
};
