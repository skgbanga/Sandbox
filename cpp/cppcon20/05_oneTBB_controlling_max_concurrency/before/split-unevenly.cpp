//==============================================================
// Copyright (c) 2020 Intel Corporation
//
// SPDX-License-Identifier: Apache-2.0
// =============================================================

#include <tbb/tbb.h>
#include <chrono>
#include <iostream>
#include <thread>

#include "../common/test_function.h"

int main() {
  const int hw_threads = std::thread::hardware_concurrency();
  tbb::task_arena ta1(2);
  tbb::task_arena ta2(hw_threads - 2);

  // warm-up
  tbb::parallel_invoke([&]() { ta1.execute([]() { run_test(); }); },
                       [&]() { ta2.execute([]() { run_test_2(); }); });

  // run test
  int num_threads_1, num_threads_2;
  double time_of_1, time_of_2;
  tbb::parallel_invoke(
      [&]() {
        ta1.execute([&]() {
          auto t0 = std::chrono::high_resolution_clock::now();
          num_threads_1 = run_test();
          time_of_1 =
              1e-9 * (std::chrono::high_resolution_clock::now() - t0).count();
        });
      },
      [&]() {
        ta2.execute([&]() {
          auto t0 = std::chrono::high_resolution_clock::now();
          num_threads_2 = run_test_2();
          time_of_2 =
              1e-9 * (std::chrono::high_resolution_clock::now() - t0).count();
        });
      });

  std::cout << "Ran test with on hw with " << hw_threads << " threads\n"
            << " test_1 ran using " << num_threads_1 << " threads in "
            << time_of_1 << " seconds.\n"
            << " test_2 ran using " << num_threads_2 << " threads in "
            << time_of_2 << " seconds.\n";
}
