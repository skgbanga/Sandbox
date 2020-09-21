#include <iostream>

void sequential() {
  std::cout << "Running sequentially:\n";
  auto st0 = std::chrono::high_resolution_clock::now();
  sleep_then_print(2, "Hello");
  sleep_then_print(1, ", TBB!");
  auto st1 = std::chrono::high_resolution_clock::now();
    
  std::cout << "\nSerial time   = " << 1e-9 * (st1-st0).count() << " seconds\n";

}

int main() {
  std::cout << "Hello world\n";
}
