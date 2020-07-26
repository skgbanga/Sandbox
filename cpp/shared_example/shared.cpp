#include "static.h"

std::string dynamic_speak() {
  return static_speak() + " I am dynamic";
}
