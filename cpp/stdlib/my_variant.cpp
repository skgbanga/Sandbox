#include <algorithm>
#include <cstddef>  // size_t
#include <cstdio>

namespace mystd {

namespace detail {

// determine the max size required to store the given type
template <typename... Args>
constexpr auto max_size() {
  size_t m = 0;
  auto l = [&](size_t sz) { m = std::max(m, sz); };
  (l(sizeof(Args)), ...);

  return m;
}

// determine the max alignment out of the given types
template <typename... Args>
constexpr auto max_alignment() {
  size_t m = 0;
  auto l = [&](size_t sz) { m = std::max(m, sz); };
  (l(std::alignment_of_v<Args>), ...);

  return m;
}

// Determine the index of a type in a parameterized pack
template <typename T, typename Head, typename... Types>
constexpr size_t index(size_t value = 0) {
  if constexpr (std::is_same_v<T, Head>) {
    return value;
  } else {
    return index<T, Types...>(value + 1);
  }
}

template <typename T>
void dtor(void* ptr) {
  reinterpret_cast<T*>(ptr)->~T();
}

template <typename Ret, typename Func, typename T>
Ret call(Func&& func, void* ptr) {
  return func(*reinterpret_cast<T*>(ptr));
}

}  // namespace detail

template <typename... Types>
class variant;

// variant size is similar to tuple_size
template <typename T>
struct variant_size;

template <typename... Types>
struct variant_size<variant<Types...>>
    : std::integral_constant<size_t, sizeof...(Types)> {};

// variant alternative is similar to tuple_element
template <size_t Index, typename T>
struct variant_alternative;

template <size_t Index, typename Head, typename... Tail>
struct variant_alternative<Index, variant<Head, Tail...>>
    : variant_alternative<Index - 1, variant<Tail...>> {};

template <typename Head, typename... Tail>
struct variant_alternative<0, variant<Head, Tail...>> {
  using type = Head;
};

template <size_t Index, typename T>
using variant_alternative_t = typename variant_alternative<Index, T>::type;

template <typename... Types>
class variant {
 public:
  variant() {
    using T = variant_alternative_t<0, variant<Types...>>;
    new (&storage_) T{};
  }

  template <typename T, typename... Args>
  variant(std::in_place_type_t<T>, Args&&... args) {
    new (&storage_) T{std::forward<Args>(args)...};
    which_ = detail::index<T, Types...>();
  }

  constexpr size_t index() const noexcept { return which_; }

  ~variant() {
    using T = void (*)(void*);
    static T funcs[sizeof...(Types)] = {detail::dtor<Types>...};
    funcs[which_](&storage_);
  }

  void* storage() { return &storage_; }

 private:
  template <typename T>
  T* get() {
    return reinterpret_cast<T*>(&storage_);
  }

  using storage_t = std::aligned_storage_t<detail::max_size<Types...>(),
                                           detail::max_alignment<Types...>()>;
  storage_t storage_;
  size_t which_ = 0;
};

namespace detail {
template <typename Visitor, typename Variant, size_t... Indexes>
auto do_visit(Visitor&& visitor,
              Variant&& variant,
              std::index_sequence<Indexes...>) {
  using VT = std::decay_t<Variant>;
  using ReturnType =
      std::invoke_result_t<Visitor, typename variant_alternative<0, VT>::type>;
  using Func = ReturnType (*)(Visitor&&, void*);
  static Func funcs[variant_size<VT>::value] = {
      detail::call<ReturnType, Visitor, variant_alternative_t<Indexes, VT>>...};
  return funcs[variant.index()](std::forward<Visitor>(visitor),
                                variant.storage());
}

}  // namespace detail

template <typename Visitor, typename Variant>
decltype(auto) visit(Visitor&& visitor, Variant&& variant) {
  using VT = std::decay_t<Variant>;
  return detail::do_visit(std::forward<Visitor>(visitor),
                          std::forward<Variant>(variant),
                          std::make_index_sequence<variant_size<VT>::value>{});
}

}  // namespace mystd

template <class... Ts>
struct overloaded : Ts... {
  using Ts::operator()...;
};
template <class... Ts>
overloaded(Ts...) -> overloaded<Ts...>;

struct Foo {
  Foo() { puts("foo"); }
  Foo(const char* s) { printf("%s\n", s); }
  ~Foo() { puts("~foo"); }
};

int main() {
  {
    mystd::variant<Foo, int> v1;
    mystd::variant<int, Foo> v2;
  }

  { mystd::variant<Foo, int> v(std::in_place_type_t<Foo>{}, "hello world"); }

  {
    mystd::variant<Foo, int> v(std::in_place_type_t<int>{}, 42);
    mystd::visit(overloaded{[](int a) { printf("contained is %d\n", a); },
                            [](const Foo&) { puts("contained is foo"); }},
                 v);
  }
}
