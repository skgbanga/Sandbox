// http://blogs.microsoft.co.il/sasha/2015/01/12/implementing-tuple-part-1/
#include <cstddef>  // size_t
#include <stdio.h>  // printf
#include <utility>

// most of detail is defined in C++14
namespace detail {

template <size_t... Indices>
struct index_sequence {
};

template <size_t I, typename Sequence>
struct cat_index_sequence {  };

template <size_t I, size_t... Indices>
struct cat_index_sequence<I, index_sequence<Indices...>> {
  using type = index_sequence<Indices..., I>;
};

template <size_t N>
struct make_index_sequence {
  using type =
    typename cat_index_sequence<N - 1, typename make_index_sequence<N - 1>::type>::type;
};

template <>
struct make_index_sequence<1> {
  using type = index_sequence<0>;
};

}

template <size_t I, typename T>
class tuple_element {
 public:
  using type = T;

  tuple_element() = default;

  template <typename U>
  tuple_element(U&& u) : value_(std::forward<U>(u)) {  }

  T value_;
};

template <typename Sequence, typename... Types>
class tuple_impl;

template <size_t ...Indices, typename... Types>
class tuple_impl<detail::index_sequence<Indices...>, Types...> : public tuple_element<Indices, Types>... {
 public:
  tuple_impl() = default;

  template <typename... Others>
  explicit tuple_impl(Others&&... others) : tuple_element<Indices, Types>(std::forward<Others>(others))... {  };
};

template <template <typename> typename Op, typename... Types>
constexpr bool is_one_of() {
  return (... || Op<Types>{});
}

template <typename... Types>
class tuple : public tuple_impl<typename detail::make_index_sequence<sizeof...(Types)>::type, Types...> {
 public:
  using base_t = tuple_impl<typename detail::make_index_sequence<sizeof...(Types)>::type, Types...>;

  tuple() = default;

  // very greedy constructor
  template <typename... Others>
  tuple(Others&&... others) : base_t(std::forward<Others>(others)...) {  }
};


// implementation of the free functions get
template <size_t I, typename Head, typename... Tail>
constexpr auto type_at_index () {
  if constexpr (I == 0) {
    return Head{};
  } else {
    return type_at_index<I - 1, Tail...>();
  }
}

template <typename T, typename... Types>
constexpr auto count_types() {
  int sum = 0;
  ((sum += std::is_same<T, Types>::value ? 1 : 0), ...);
  return sum;
}

template <typename T, typename Head, typename... Tail>
constexpr auto index_of_type(size_t sofar = 0) {
  if constexpr (std::is_same<T, Head>::value) {
    return sofar;
  } else {
    return index_of_type<T, Tail...>(sofar + 1);
  }
}

template <size_t I, typename... Types>
auto get(const tuple<Types...>& value) {
  const tuple_element<I, decltype(type_at_index<I, Types...>())>& t = value;
  return t.value_;
}

template <typename T, typename... Types>
auto get(const tuple<Types...>& value) {
  static_assert(count_types<T, Types...>() == 1);
  return get<index_of_type<T, Types...>()>(value);
}

template <typename... Types>
auto make_tuple(Types&&... types) {
  return tuple<Types...>(std::forward<Types>(types)...);
}

template <typename T>
struct tuple_size;

template <typename... Types>
struct tuple_size<tuple<Types...>> : std::integral_constant<size_t, sizeof...(Types)> {  };

template <typename... Types>
struct tuple_size<tuple<Types...>&> : std::integral_constant<size_t, sizeof...(Types)> {  };

template <typename Tuple>
auto cat_tuple(Tuple&& tuple) {
  return std::forward<Tuple>(tuple);
} 

template <typename Tuple1, size_t... Indices1, typename Tuple2, size_t... Indices2>
auto cat_tuple2_impl(Tuple1&& t1, Tuple2&& t2, detail::index_sequence<Indices1...>, detail::index_sequence<Indices2...>) {
  return make_tuple(get<Indices1>(t1)..., get<Indices2>(t2)...);
}

template <typename Tuple1, typename Tuple2>
auto cat_tuple2(Tuple1&& t1, Tuple2&& t2) {
  return cat_tuple2_impl(t1, t2, typename detail::make_index_sequence<tuple_size<Tuple1>::value>::type(),
      typename detail::make_index_sequence<tuple_size<Tuple2>::value>::type());
}

template <typename Tuple1, typename Tuple2, typename... Rest>
auto cat_tuple(Tuple1&& t1, Tuple2&& t2, Rest&&... rest) {
  return cat_tuple(cat_tuple2(std::forward<Tuple1>(t1), std::forward<Tuple2>(t2)),
                   std::forward<Rest>(rest)...);
}

int main() {
  tuple<int, double> t (42, 2.0);
  printf("%lu\n", tuple_size<decltype(t)>::value);

  printf("%d\n", get<0>(t));
  printf("%0.2f\n", get<1>(t));

  printf("%d\n", get<int>(t));
  tuple<int, double> t2 (1, 2);
  auto t3 = cat_tuple(t, t2);

  printf("%lu\n", tuple_size<decltype(t3)>::value);
  printf("%d\n", get<0>(t3));
  printf("%0.2f\n", get<1>(t3));
  printf("%d\n", get<2>(t3));
  printf("%0.2f\n", get<3>(t3));
}
