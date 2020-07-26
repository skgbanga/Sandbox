#include "spinlock_mutex.h"

#include <atomic>
#include <cassert>
#include <chrono>
#include <cmath>
#include <future>
#include <iomanip>
#include <iostream>
#include <thread>
#include <utility>

// platform specific
#include <pthread.h>    // for setting affinity
#include <x86intrin.h>  // for getting __rdtsc()

// Objective:
// Build a 4 * 2 matrix of the following structure
//
//                                                 atomic_counter       counter_with_mutex
// (same logical core)
// (same physical core, different logical core)
// (different physical core, same socket)
// (different physical core, different numa)
//
// The idea is to see how much can counter increase in a certain interval for above scenarios

namespace util {

   // code obtained using man pthreads*
   // renewed respect for c developers along the process
   static void display_sched_attr(std::thread::native_handle_type native)
   {
      int policy = 0;
      sched_param param;
      __attribute__((unused)) int ret = pthread_getschedparam(native, &policy, &param);
      assert(ret == 0);

      printf("    policy=%s, priority=%d\n",
            (policy == SCHED_FIFO)  ? "SCHED_FIFO" :
            (policy == SCHED_RR)    ? "SCHED_RR" :
            (policy == SCHED_OTHER) ? "SCHED_OTHER" :
            "???",
            param.sched_priority);
   }

   static void display_affinity(std::thread::native_handle_type native)
   {
      cpu_set_t cpuset;
      CPU_ZERO(&cpuset);
      __attribute__((unused)) int ret = pthread_getaffinity_np(native, sizeof(cpu_set_t), &cpuset);
      assert(ret == 0);


      printf("Set returned by pthread_getaffinity_np() contained:\n");
      for (int j = 0; j < CPU_SETSIZE; j++)
      {
         if (CPU_ISSET(j, &cpuset))
            printf("    CPU %d\n", j);
      }
   }

   static void set_affinity(std::thread::native_handle_type native, int core_id)
   {
      assert(core_id < CPU_SETSIZE);
      assert(core_id >= 0);

      cpu_set_t cpuset;
      CPU_ZERO(&cpuset);
      CPU_SET(core_id, &cpuset);
      __attribute__((unused)) int ret = pthread_setaffinity_np(native, sizeof(cpu_set_t), &cpuset);
      assert(ret == 0);
   }
} // end namespace util

static const uint64_t interval = 3 * std::pow(10, 9);

// 1 thread, 1 core
template <typename T>
void increment_single_thread(T& counter)
{
   uint64_t end = __rdtsc() + interval;
   while (__rdtsc() < end)
      ++counter;
}

// overload for std::atomic so that fetch_add can be called with std::memory_order_relaxed
void increment_single_thread(std::atomic<uint64_t>& counter)
{
   uint64_t end = __rdtsc() + interval;
   while (__rdtsc() < end)
      counter.fetch_add(1, std::memory_order_relaxed);
}

template <typename T, typename LockType>
void increment_with_lock(T& counter, LockType& mut)
{
   uint64_t end = __rdtsc() + interval;
   while (__rdtsc() < end)
   {
      std::lock_guard<LockType> guard(mut);
      ++counter;
   }
}

// 2 threads, same core
template <typename T>
void increment_two_threads(T& counter, bool use_lock, int core_id1, int core_id2)
{
   // mutex to synchronize the start of the two threads
   std::mutex mut;
   std::condition_variable cv;
   std::atomic_bool ready {false};

   // mutex to synchronize incrementing the counter
   std::mutex run_mut;

   auto threadFunc = [&](bool use_lock)
   {
      // wait for the parent thread to set the affinity
      std::unique_lock<std::mutex> lk(mut);
      cv.wait(lk, [&] { return ready.load(); });
      lk.unlock();

      if (use_lock)
         increment_with_lock(counter, run_mut);
      else
         increment_single_thread(counter);
   };

   std::thread t1(threadFunc, use_lock);
   std::thread t2(threadFunc, use_lock);

   util::set_affinity(t1.native_handle(), core_id1);
   util::set_affinity(t2.native_handle(), core_id2);

   ready = true;
   cv.notify_all();

   // wait for threads to complete
   t1.join();
   t2.join();
}

int main()
{
   // single thread
   uint64_t counter = 0;
   increment_single_thread(counter);
   std::cout.imbue(std::locale("")); // to print the commas between numbers
   std::cout << "Single thread: " << counter << '\n';

   // single thread with std::mutex
   uint64_t counter_lock = 0;
   std::mutex mut_single;
   increment_with_lock(counter_lock, mut_single);
   std::cout << "Single thread with std::mutex: " << counter_lock << '\n';

   // single thread with spin lock
   uint64_t spin_counter = 0;
   blog::spinlock_mutex spin_mutex;
   increment_with_lock(spin_counter, spin_mutex);
   std::cout << "Single thread with spin lock is " << spin_counter << '\n';

   auto local = [](std::string str, int core1, int core2)
   {
      std::atomic<uint64_t> atomic_counter {0};
      uint64_t lock_counter = 0;

      increment_two_threads(atomic_counter, false, core1, core2);
      increment_two_threads(lock_counter, true, core1, core2);

      std::cout << str << std::setw(12) << atomic_counter.load() << std::setw(12) << lock_counter << '\n';
   };

   // Do "lscpu -e" to get all the below information on your machine
   // TODO Should we set the scheduling policy/priority?

   std::cout << "Case1: Same logical core" << '\n';
   std::cout << "Case2: Same physical core, different hyper-threads" << '\n';
   std::cout << "Case3: Different physical core, same numa" << '\n';
   std::cout << "Case4: Different physical core, different numa" << '\n';

   std::cout << "       " << std::setw(12) << "atomic" << std::setw(12) << "lock" << '\n';
   local("Case1: ", 3, 3); // same logical core
   local("Case2: ", 3, 7); // same physical core, but different hyper-threads
   local("Case3: ", 2, 3); // different physical core, same socket/numa
   // local("Case4: ", 0, 0); // different physical core, different socket - My machine doesn't have this :(
}
