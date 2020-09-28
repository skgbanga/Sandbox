// https://youtu.be/jKtbNvCT8Dc

#pragma once

#include <Eigen/Dense>
#include <string>
#include <vector>

class ETL {
  std::string dataset_;
  std::string delimiter_;
  bool header_;

 public:
  ETL(std::string data, std::string separator, bool header)
      : dataset_(std::move(data)),
        delimiter_(std::move(separator)),
        header_(header) {}

  std::vector<std::vector<std::string>> readCSV();
};
