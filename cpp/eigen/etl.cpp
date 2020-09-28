#include "etl.h"

#include <fstream>

template <typename F>
void split(std::string_view strv, std::string_view delim, F f) {
  while (!strv.empty()) {
    auto found = strv.find_first_of(delim);
    if (found == std::string_view::npos) {
      f(strv);
      strv.remove_prefix(strv.size());
      return;
    } else {
      f(strv.substr(0, found));
      strv.remove_prefix(found + 1);
    }
  }
  f(strv);
}

std::vector<std::string> split(const std::string& s, std::string delim) {
  std::vector<std::string> output;

  split(std::string_view{s}, std::string_view{delim},
        [&](auto sv) { output.push_back(std::string(sv)); });

  return output;
}

std::vector<std::vector<std::string>> ETL::readCSV() {
  std::ifstream file(dataset_);
  std::vector<std::vector<std::string>> data;
  std::string line = "";
  while (getline(file, line)) {
    data.push_back(split(line, delimiter_));
  }
  return data;
}
