#include "mylib.h"

#include "fmt/format.h"

int prev_value() {
  return 41;
}

int MyGenerator::nextValue() {
  return prev_value() + 1;
}
