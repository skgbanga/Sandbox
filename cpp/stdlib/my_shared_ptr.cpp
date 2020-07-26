#include <atomic>
#include <exception>

namespace mystd {

template <typename T> class shared_ptr;

namespace detail {

struct ReferenceBlockBase {
  std::atomic<int> strongs;
  std::atomic<int> weaks;

  ReferenceBlockBase() : strongs(1), weaks(1) {}

  virtual ~ReferenceBlockBase() {}
  virtual void destroy_resource() = 0;
};
static_assert(sizeof(ReferenceBlockBase) == 16);

template <typename Ptr> struct ReferenceBlock : ReferenceBlockBase {
private:
  Ptr p_ = nullptr;

public:
  ReferenceBlock(Ptr p) : ReferenceBlockBase(), p_(p) {}
  void destroy_resource() override {
    delete p_;
    p_ = nullptr;
  }
};
template <typename Ptr> ReferenceBlock(Ptr)->ReferenceBlock<Ptr>;

template <typename Ptr, typename Deleter>
struct DeleterReferenceBlock : ReferenceBlockBase {
private:
  Ptr p_ = nullptr;
  Deleter d_;

public:
  DeleterReferenceBlock(Ptr p, Deleter d) : p_(p), d_(d) {}
  void destroy_resource() override {
    d_(p_);
    p_ = nullptr;
  }
};
template <typename Ptr, typename Deleter>
DeleterReferenceBlock(Ptr, Deleter)->DeleterReferenceBlock<Ptr, Deleter>;

template <typename T> void assign_ptr_to_sp(shared_ptr<T> &sp, T *ptr);

} // namespace detail

class bad_weak_ptr : std::exception {
  virtual const char *what() const noexcept { return "bad weak ptr"; }
};

template <typename T> struct weak_ptr;

template <typename T> class shared_ptr {
public:
  using element_type = T;

  shared_ptr() = default;     // (1)
  shared_ptr(std::nullptr_t); // (2)

  shared_ptr(const shared_ptr &other);
  shared_ptr &operator=(const shared_ptr &other);
  shared_ptr(shared_ptr &&other);
  shared_ptr &operator=(shared_ptr &&other);
  ~shared_ptr();

  template <typename Y> shared_ptr(Y *value); // (3)
  template <typename Y, typename Deleter>
  shared_ptr(Y *value, Deleter D);                                   // (4)
  template <typename Deleter> shared_ptr(std::nullptr_t, Deleter D); // (5)

  template <typename Y> shared_ptr(const shared_ptr<Y> &r, element_type *ptr);

  template <typename Y> shared_ptr(const weak_ptr<Y> &other); // throw

  // Modifers
  void reset() noexcept;
  void swap(shared_ptr &other) noexcept;

  // observers
  element_type *get() const noexcept;
  T &operator*() const noexcept;
  T *operator->() const noexcept;

  long use_count() const noexcept;
  explicit operator bool() const noexcept;

private:
  template <typename Y> shared_ptr(const weak_ptr<Y> &other, bool should_throw);

  template <class Y> friend class shared_ptr;
  template <class Y> friend struct weak_ptr;

  template <typename Y, typename... Args>
  friend shared_ptr<Y> make_shared(Args &&... args);

  template <typename Y>
  friend void detail::assign_ptr_to_sp(shared_ptr<Y> &sp, Y *ptr);

  detail::ReferenceBlockBase *block_ = nullptr;
  T *ptr_ = nullptr;
};
static_assert(sizeof(shared_ptr<int>) == 16);

template <typename T> shared_ptr<T>::shared_ptr(std::nullptr_t) {}

template <typename T>
template <typename Y>
shared_ptr<T>::shared_ptr(Y *value) {
  block_ = new detail::ReferenceBlock(value);
  detail::assign_ptr_to_sp(*this, value);
}

template <typename T>
template <typename Y, typename Deleter>
shared_ptr<T>::shared_ptr(Y *value, Deleter d) {
  block_ = new detail::DeleterReferenceBlock(value, d);
  detail::assign_ptr_to_sp(*this, value);
}

template <typename T>
template <typename Deleter>
shared_ptr<T>::shared_ptr(std::nullptr_t, Deleter d) {
  block_ = new detail::DeleterReferenceBlock(nullptr, d);
  detail::assign_ptr_to_sp(*this, (T *)nullptr);
}

template <typename T>
template <typename Y>
shared_ptr<T>::shared_ptr(const shared_ptr<Y> &r, element_type *ptr) {
  if (r.block_) {
    block_ = r.block_;
    block_->strongs += 1;
  }
  detail::assign_ptr_to_sp(*this, ptr);
}

template <typename T>
template <typename Y>
shared_ptr<T>::shared_ptr(const weak_ptr<Y> &other)
    : shared_ptr<T>(other, true) {}

// private constructor
template <typename T>
template <typename Y>
shared_ptr<T>::shared_ptr(const weak_ptr<Y> &other, bool should_throw) {
  if (!other.block_) {
    if (should_throw) {
      throw bad_weak_ptr();
    } else {
      return;
    }
  }

  auto update_strong_cnt = [&] {
    auto strong_count = other.block_->strongs.load();
    while (true) {
      if (strong_count == 0) {
        return strong_count;
      }

      if (other.block_->strongs.compare_exchange_weak(
              strong_count, strong_count + 1, std::memory_order_relaxed,
              std::memory_order_relaxed)) {
        return strong_count;
      }
    }
  };

  if (update_strong_cnt() == 0) {
    if (should_throw) {
      throw bad_weak_ptr();
    } else {
      return;
    }
  }

  block_ = other.block_;
  ptr_ = other.ptr_;
}

template <typename T> shared_ptr<T>::shared_ptr(const shared_ptr<T> &other) {
  if (other.block_) {
    block_ = other.block_;
    ptr_ = other.ptr_;
    block_->strongs++;
  }
}

template <typename T>
shared_ptr<T> &shared_ptr<T>::operator=(const shared_ptr<T> &other) {
  if (block_) {
    reset();
  }

  block_ = other.block_;
  ptr_ = other.ptr_;
  block_->strongs++;

  return *this;
}

template <typename T> shared_ptr<T>::shared_ptr(shared_ptr &&other) {
  block_ = other.block_;
  ptr_ = other.ptr_;

  other.block_ = nullptr;
  other.ptr_ = nullptr;
}

template <typename T>
shared_ptr<T> &shared_ptr<T>::operator=(shared_ptr<T> &&other) {
  if (block_) {
    reset();
  }

  block_ = other.block_;
  ptr_ = other.ptr_;

  other.block_ = nullptr;
  other.ptr_ = nullptr;
  return *this;
}

template <typename T> shared_ptr<T>::~shared_ptr() { reset(); }

template <typename T> void shared_ptr<T>::reset() noexcept {
  if (!block_) {
    return;
  }

  auto prev_strong = block_->strongs.fetch_sub(1);
  if (prev_strong == 1) {
    block_->destroy_resource();
    auto prev_weak = block_->weaks.fetch_sub(1);
    if (prev_weak == 1) {
      delete block_;
    }
  }

  block_ = nullptr;
  ptr_ = nullptr;
}

template <typename T> void shared_ptr<T>::swap(shared_ptr<T> &other) noexcept {
  using std::swap;
  swap(block_, other.block_);
  swap(ptr_, other.ptr_);
}

template <typename T> long shared_ptr<T>::use_count() const noexcept {
  if (!block_) {
    return 0;
  }

  return block_->strongs;
}

template <typename T>
typename shared_ptr<T>::element_type *shared_ptr<T>::get() const noexcept {
  return ptr_;
}

template <typename T> T &shared_ptr<T>::operator*() const noexcept {
  return *ptr_;
}

template <typename T> T *shared_ptr<T>::operator->() const noexcept {
  return ptr_;
}

template <typename T> shared_ptr<T>::operator bool() const noexcept {
  return ptr_ != nullptr;
}

/* Free functions */

namespace detail {

template <typename T> struct SharedReferenceBlock : ReferenceBlockBase {

  template <typename... Args>
  SharedReferenceBlock(Args &&... args) : ReferenceBlockBase() {
    new ((void *)&storage) T{std::forward<Args>(args)...};
  }

  void destroy_resource() override {
    auto t = reinterpret_cast<T *>(&storage);
    t->~T();
  }

  std::aligned_storage_t<sizeof(T), alignof(T)> storage;
};

} // namespace detail

template <typename T, typename... Args>
shared_ptr<T> make_shared(Args &&... args) {
  auto phantom =
      new detail::SharedReferenceBlock<T>(std::forward<Args>(args)...);

  shared_ptr<T> sp;
  sp.block_ = phantom;
  detail::assign_ptr_to_sp(sp, reinterpret_cast<T *>(&phantom->storage));

  return sp;
}

template <typename T> struct weak_ptr {
  using element_type = T;

  weak_ptr() = default;
  weak_ptr(const weak_ptr &other);
  weak_ptr &operator=(const weak_ptr &other);
  weak_ptr(weak_ptr &&other);
  weak_ptr &operator=(weak_ptr &&other);
  ~weak_ptr();

  template <typename Y> weak_ptr(const shared_ptr<Y> &other) noexcept;
  template <typename Y>
  weak_ptr &operator=(const shared_ptr<Y> &other) noexcept;

  void reset() noexcept;
  shared_ptr<T> lock() const noexcept;
  long use_count() const noexcept;
  bool expired() const noexcept;

private:
  template <class Y> friend class shared_ptr;

  detail::ReferenceBlockBase *block_ = nullptr;
  element_type *ptr_ = nullptr;
};

template <typename T> weak_ptr<T>::weak_ptr(const weak_ptr &other) {
  if (other.block_) {
    block_ = other.block_;
    ptr_ = other.ptr_;
    block_->weaks++;
  }
}

template <typename T>
weak_ptr<T> &weak_ptr<T>::operator=(const weak_ptr &other) {
  if (block_) {
    reset();
  }

  block_ = other.block_;
  ptr_ = other.ptr_;

  block_->weaks++;
  return *this;
}

template <typename T> weak_ptr<T>::weak_ptr(weak_ptr &&other) {
  block_ = other.block_;
  ptr_ = other.ptr_;

  other.block_ = nullptr;
  other.ptr_ = nullptr;
}

template <typename T> weak_ptr<T> &weak_ptr<T>::operator=(weak_ptr &&other) {
  if (block_) {
    reset();
  }

  block_ = other.block_;
  ptr_ = other.ptr_;

  other.block_ = nullptr;
  other.ptr_ = nullptr;
  return *this;
}

template <typename T> weak_ptr<T>::~weak_ptr() { reset(); }

template <typename T>
template <typename Y>
weak_ptr<T>::weak_ptr(const shared_ptr<Y> &other) noexcept {
  if (other.block_) {
    block_ = other.block_;
    ptr_ = other.ptr_;
    block_->weaks++;
  }
}

template <typename T>
template <typename Y>
weak_ptr<T> &weak_ptr<T>::operator=(const shared_ptr<Y> &other) noexcept {
  if (block_) {
    reset();
  }

  if (other.block_) {
    block_ = other.block_;
    ptr_ = other.ptr_;
    block_->weaks++;
  }

  return *this;
}

template <typename T> void weak_ptr<T>::reset() noexcept {
  if (!block_) {
    return;
  }

  auto prev_weak = block_->weaks.fetch_sub(1);
  if (prev_weak == 1) {
    delete block_;
  }

  block_ = nullptr;
  ptr_ = nullptr;
}

template <typename T> long weak_ptr<T>::use_count() const noexcept {
  if (!block_) {
    return 0;
  }

  return block_->strongs;
}

template <typename T> bool weak_ptr<T>::expired() const noexcept {
  return use_count() == 0;
}

template <typename T> shared_ptr<T> weak_ptr<T>::lock() const noexcept {
  return shared_ptr<T>(*this, false);
}

template <typename T> class enable_shared_from_this {
public:
  shared_ptr<T> shared_from_this() { return weak_this.lock(); }

private:
  template <typename Y>
  friend void detail::assign_ptr_to_sp(shared_ptr<Y> &sp, Y *ptr);

  weak_ptr<T> weak_this;
};

namespace detail {

template <typename T> void assign_ptr_to_sp(shared_ptr<T> &sp, T *ptr) {
  sp.ptr_ = ptr;
  if constexpr (std::is_base_of_v<enable_shared_from_this<T>, T>) {
    enable_shared_from_this<T> *base = ptr;
    base->weak_this = weak_ptr<T>(sp);
  }
}

} // namespace detail

} // namespace mystd

#include <cstdio>
#include <new>
#include <thread>
#include <utility>

#include <cassert>
#include <condition_variable>
#include <mutex>

struct Foo {
  static int counter;

  Foo() : mycount(counter++) { printf("Foo %d\n", mycount); }
  Foo(const Foo &) { printf("Foo(const Foo&) %d\n", mycount); }
  ~Foo() { printf("~Foo %d\n", mycount); }

  int mycount;
};
int Foo::counter = 0;

int main() {
  auto l = [](const char *str, auto exp) {
    puts(str);
    exp();
    puts("");
  };

  l("Default construction", [] { mystd::shared_ptr<int> sp; });

  l("Construction from a T*", [] {
    Foo *f = new Foo();
    mystd::shared_ptr<Foo> sp(f);
  });

  l("Copy construction", [] {
    Foo *f = new Foo();
    mystd::shared_ptr<Foo> sp(f);
    auto sp2 = sp;
    printf("Two shared ptrs are pointing to Foo now\n");
  });

  l("Copy construction from default ", [] {
    mystd::shared_ptr<Foo> sp;
    auto sp2 = sp;
    puts("default shared ptr has been copied");
  });

  l("Copy assignment - assign to default constructed sp", [] {
    Foo *f = new Foo();
    mystd::shared_ptr<Foo> sp1;
    mystd::shared_ptr<Foo> sp2(f);
    sp1 = sp2;
    puts("Assigned to a defaulted sp");
  });

  l("Copy assignment - assign to last owning shared ptr", [] {
    Foo *f1 = new Foo();
    Foo *f2 = new Foo();
    mystd::shared_ptr<Foo> sp1(f1);
    mystd::shared_ptr<Foo> sp2(f2);
    sp1 = sp2;
    puts("Assigned to last owning sp");
  });

  l("Copy assignment - assign to multiple owned sp", [] {
    Foo *f1 = new Foo();
    Foo *f2 = new Foo();
    mystd::shared_ptr<Foo> sp1(f1);
    auto sp3 = sp1;

    mystd::shared_ptr<Foo> sp2(f2);
    sp1 = sp2;
    puts("Assigned to multiple owned sp");
  });

  l("Move constructor", [] {
    Foo *f1 = new Foo();
    mystd::shared_ptr<Foo> sp(f1);
    auto f2 = std::move(sp);
    puts("Moved singly owned sp");
  });

  l("Move assignment - assign to default constructed sp", [] {
    Foo *f = new Foo();
    mystd::shared_ptr<Foo> sp1;
    mystd::shared_ptr<Foo> sp2(f);
    sp1 = std::move(sp2);
    puts("Assigned to a defaulted sp");
  });

  l("Move assignment - assign to last owning shared ptr", [] {
    Foo *f1 = new Foo();
    Foo *f2 = new Foo();
    mystd::shared_ptr<Foo> sp1(f1);
    mystd::shared_ptr<Foo> sp2(f2);
    sp1 = std::move(sp2);
    puts("Assigned to last owning sp");
  });

  l("Move assignment - assign to multiple owned sp", [] {
    Foo *f1 = new Foo();
    Foo *f2 = new Foo();
    mystd::shared_ptr<Foo> sp1(f1);
    auto sp3 = sp1;

    mystd::shared_ptr<Foo> sp2(f2);
    sp1 = std::move(sp2);
    puts("Assigned to multiple owned sp");
  });

  l("Custom Deleter", [] {
    Foo *f = new Foo();
    mystd::shared_ptr<Foo> sp(f, [](Foo *f) {
      puts("Calling custom deleter");
      delete f;
    });
  });

  l("Custom Deleter with nullptr", [] {
    mystd::shared_ptr<Foo> sp(nullptr, [](Foo *f) {
      puts("Calling custom deleter");
      delete f;
    });
    printf("use count is %ld\n", sp.use_count());
  });

  l("Use count ", [] {
    Foo *f = new Foo();
    mystd::shared_ptr<Foo> sp(f);
    printf("Use count %ld\n", sp.use_count());
    auto sp2 = sp;
    printf("Use count %ld\n", sp.use_count());
  });

  l("Aliasing constructor", [] {
    Foo *f = new Foo();
    mystd::shared_ptr<Foo> sp(f);
    {
      mystd::shared_ptr<int> spint(sp, &f->mycount);
      printf("Get of aliased guy returned %d\n", *spint.get());
      printf("use count with aliased ptr is %ld\n", spint.use_count());
    }
    puts("Aliased ptr is dead");
  });

  l("Aliasing constructor - 2", [] {
    int i;
    mystd::shared_ptr<int> sp(mystd::shared_ptr<Foo>(), &i);
    assert(sp.use_count() == 0);
    assert(sp); // weird quirk of aliasing constructor
  });

  l("reset without argument", [] {
    Foo *f = new Foo();
    mystd::shared_ptr<Foo> sp(f);
    sp.reset();
    puts("reset has been called");
  });

  l("reset default constructed", [] {
    mystd::shared_ptr<Foo> sp;
    sp.reset();
  });

  l("operator bool", [] {
    mystd::shared_ptr<Foo> sp;
    assert(!sp);

    sp = mystd::shared_ptr<Foo>(new Foo());
    assert(sp);

    auto sp2 = std::move(sp);
    assert(sp2);
    assert(!sp);
  });

  l("make shared", [] {
    auto sp = mystd::make_shared<Foo>();
    puts("make share has been called");
  });

  l("make shared reset", [] {
    auto sp = mystd::make_shared<Foo>();
    sp.reset();
    assert(!sp);
    puts("Checked boolean opeartor");
  });

  l("multi threaded shared ptr destruction", [] {
    std::mutex m;
    std::condition_variable cv;
    bool ready = false;

    ready = false;

    auto sp = mystd::make_shared<Foo>();
    printf("Main thread %ld\n", pthread_self());

    auto t1 = std::thread([&, sp2 = sp]() mutable {
      {
        std::unique_lock lock(m);
        ready = true;
      }
      cv.notify_all();
      using namespace std::chrono_literals;
      std::this_thread::sleep_for(3us);

      sp2.reset();
    });

    {
      std::unique_lock lock(m);
      cv.wait(lock, [&] { return ready; });
    }
    sp.reset();

    t1.join();
  });

  l("weak ptr", [] {
    mystd::weak_ptr<Foo> wp;
    {
      auto sp = mystd::make_shared<Foo>();
      wp = sp;
    }
    puts("weak ptr exists here");
  });

  l("strong/weak exception", [] {
    mystd::weak_ptr<Foo> wp;
    try {
      mystd::shared_ptr<Foo> sp(wp);
    } catch (const mystd::bad_weak_ptr &bwp) {
      puts("Exception thrown");
    }
  });

  l("empty weak ptr lock", [] {
    mystd::weak_ptr<Foo> wp;
    auto sp = wp.lock();
    assert(!sp);
    puts("shared ptr empty");
  });

  l("weak ptr lock", [] {
    auto sp = mystd::make_shared<Foo>();
    mystd::weak_ptr<Foo> wp(sp);

    auto sp2 = wp.lock();
    assert(sp2);
    assert(sp2.use_count() == 2);
  });

  l("enable_shared_from_this", [] {
    struct Bar : mystd::enable_shared_from_this<Bar> {
      Bar(int i) : i_(i) {}
      int i_;
    };

    auto sp = mystd::make_shared<Bar>(42);
    auto sp2 = sp->shared_from_this();
    assert(sp.use_count() == 2);
  });
}
