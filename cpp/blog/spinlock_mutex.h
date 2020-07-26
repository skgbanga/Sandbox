#pragma once

#include <atomic>

// Implementation borrowed from Anthony Williams' C++ concurrency in action
namespace blog
{
   class spinlock_mutex
   {
      std::atomic_flag flag;
      public:
      spinlock_mutex() :
         flag(ATOMIC_FLAG_INIT)
      {  }

      void lock()
      {
         while (flag.test_and_set(std::memory_order_acquire));
      }

      void unlock()
      {
         flag.clear(std::memory_order_release);
      }
   };

} // end namespace blog
