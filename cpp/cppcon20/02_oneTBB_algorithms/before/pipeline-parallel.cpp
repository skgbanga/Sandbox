//==============================================================
// Copyright (c) 2020 Intel Corporation
//
// SPDX-License-Identifier: Apache-2.0
// =============================================================

#include <algorithm>
#include <cctype>
#include <chrono>
#include <fstream>
#include <iostream>
#include <memory>
#include <string>
#include <thread>

#include <tbb/tbb.h>

#define INCORRECT_VALUE 1
#define INCORRECT_MODE_A tbb::filter_mode::serial_in_order
#define INCORRECT_MODE_B tbb::filter_mode::parallel

using CaseStringPtr = std::shared_ptr<std::string>;

//
// These functions are defined in common/case.cpp
//
void initCaseChange(int num_strings, int string_len, int free_list_size);
CaseStringPtr getCaseString(std::ofstream& f);
void writeCaseString(std::ofstream& f, CaseStringPtr s);

void change_case_parallel(int num_tokens,
                          std::ofstream& caseBeforeFile,
                          std::ofstream& caseAfterFile) {
  tbb::parallel_pipeline(
      num_tokens, tbb::make_filter<void, CaseStringPtr>(
                      tbb::filter::mode::serial_in_order,
                      [&](tbb::flow_control& fc) -> CaseStringPtr {
                        CaseStringPtr s_ptr = getCaseString(caseBeforeFile);
                        if (!s_ptr)
                          fc.stop();
                        return s_ptr;
                      }) &
                      tbb::make_filter<CaseStringPtr, CaseStringPtr>(
                          tbb::filter::mode::parallel,
                          [](CaseStringPtr s_ptr) -> CaseStringPtr {
                            std::transform(s_ptr->begin(), s_ptr->end(),
                                           s_ptr->begin(), [](char c) -> char {
                                             if (std::islower(c))
                                               return std::toupper(c);
                                             else if (std::isupper(c))
                                               return std::tolower(c);
                                             else
                                               return c;
                                           });
                            return s_ptr;
                          }) &
                      tbb::make_filter<CaseStringPtr, void>(
                          tbb::filter::mode::serial_in_order,
                          [&](CaseStringPtr s_ptr) -> void {
                            writeCaseString(caseAfterFile, s_ptr);
                          }));
}

static void warmupTBB() {
  int num_threads = std::thread::hardware_concurrency();
  tbb::parallel_for(0, num_threads, [](unsigned int) {
    std::this_thread::sleep_for(std::chrono::milliseconds(10));
  });
}

int main() {
  int num_tokens = std::thread::hardware_concurrency();
  int num_strings = 100;
  int string_len = 100000;
  int free_list_size = num_tokens;

  std::ofstream caseBeforeFile("parallel_pipeline_before.txt");
  std::ofstream caseAfterFile("parallel_pipeline_after.txt");
  initCaseChange(num_strings, string_len, free_list_size);

  warmupTBB();
  double parallel_time = 0.0;
  {
    auto pt0 = std::chrono::high_resolution_clock::now();
    change_case_parallel(num_tokens, caseBeforeFile, caseAfterFile);
    parallel_time =
        1e-9 * (std::chrono::high_resolution_clock::now() - pt0).count();
  }
  std::cout << "parallel_time == " << parallel_time << " seconds" << std::endl;
  return 0;
}