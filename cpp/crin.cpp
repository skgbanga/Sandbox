//  g++ -std=c++20 -fcoroutines -g crin.cpp

#include <cassert>
#include <coroutine>
#include <functional>

#include <cstdio>

namespace crin {

template <typename T>
struct task {
  struct promise;
  using handle_type = std::coroutine_handle<promise>;

  struct final_awaitable {
    bool await_ready() { return false; }
    void await_resume() noexcept {}
    std::coroutine_handle<> await_suspend(handle_type coro) {
      printf("final suspend await suspend on %p\n", coro.address());
      auto continuation = coro.promise().continuation_;
      return (continuation) ? continuation : std::noop_coroutine();
    }
  };

  struct promise {
    std::coroutine_handle<> continuation_ = nullptr;

    auto get_return_object() {
      return std::coroutine_handle<promise>::from_promise(*this);
    }
    auto initial_suspend() noexcept { return std::suspend_always{}; }
    auto final_suspend() noexcept {
      printf("final suspend being called.");
      if (continuation_) {
        printf(" have continuation %p\n", continuation_.address());
      } else {
        puts(" no continuation");
      }
      return final_awaitable{};
    }
    void unhandled_exception() { std::terminate(); }
  };

  using promise_type = promise;

  struct awaitable {
    awaitable(handle_type handle) : handle_(handle) {}

    bool await_ready() {
      printf("await ready called on %p\n", handle_.address());
      return handle_.done();
    }
    handle_type await_resume() noexcept {
      printf("await resume called on %p\n", handle_.address());
      handle_.resume();
      return handle_;
    }
    handle_type await_suspend(std::coroutine_handle<> awaiting_coro) {
      printf("current coro %p\n", handle_.address());
      printf("setting coroutine %p\n", awaiting_coro.address());
      handle_.promise().continuation_ = awaiting_coro;
      return handle_;
    }

   private:
    handle_type handle_ = nullptr;
  };

  task(handle_type handle) noexcept : handle_(handle) {
    printf("constructing a coro %p\n", handle_.address());
  }
  task(const task&) = delete;
  task& operator=(const task&) = delete;
  task(task&& other) : handle_(other.handle_) { other.handle_ = nullptr; }
  task& operator=(task&& other) {
    reset();
    handle_ = other.handle_;
    other.handle_ = nullptr;
  }
  ~task() noexcept { reset(); }

  bool done() { return handle_.done(); }
  void resume() {
    assert(!done());
    handle_.resume();
  }
  auto operator co_await() const& noexcept { return awaitable{handle_}; }

  void reset() {
    if (handle_) {
      handle_.destroy();
    }
  }
  handle_type handle_ = nullptr;
};

template <typename Func, typename... Args>
void run(Func&& func, Args&&... args) {
  auto task =
      std::invoke(std::forward<Func>(func), std::forward<Args>(args)...);
  while (!task.done()) {
    printf("==== calling resume on %p =====\n", task.handle_.address());
    task.resume();
  }
}

}  // namespace crin

crin::task<void> countdown(int n) {
  while (n > 0) {
    printf("%d\n", n);
    co_await std::suspend_always();
    n -= 1;
  }
}

crin::task<void> do_countdown() {
  puts("In do countdown");
  co_await countdown(5);
}

int main() {
  crin::run(do_countdown);
}
