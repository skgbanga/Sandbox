#include "etl.h"


int main(int argc, char** argv) {
  ETL etl(argv[1], argv[2], argv[3]);
  auto dataset = etl.readCSV();
}
