#ifndef ICARUS_BASE_TRAVERSE_H
#define ICARUS_BASE_TRAVERSE_H

#include <tuple>
#include <utility>

#include "base/debug.h"
#include "base/meta.h"

namespace base {

namespace internal_traverse {

template <typename T, typename Tr>
concept DirectlyTraversableBy = std::invocable<Tr, T&> or requires(T t, Tr tr) {
  BaseTraverse(tr, t);
};

template <typename T, typename Tr>
concept TryTupleTraversableBy =
    not DirectlyTraversableBy<T, Tr> and SatisfiesTupleProtocol<T>;

template <typename T, typename Tr>
concept TryContainerTraversableBy =
    not DirectlyTraversableBy<T, Tr> and not TryTupleTraversableBy<T, Tr> and
    Container<T>;

template <typename Tr, typename T>
struct TraversableTrait : std::false_type {};

template <typename Tr, DirectlyTraversableBy<Tr> T>
struct TraversableTrait<Tr, T> : std::true_type {};

template <typename Tr, typename T, typename Seq>
struct TraversableTupleTrait;
template <typename Tr, typename T, size_t... Ns>
struct TraversableTupleTrait<Tr, T, std::index_sequence<Ns...>>
    : std::integral_constant<
          bool, (TraversableTrait<
                     Tr, typename std::tuple_element<Ns, T>::type>::value and
                 ...)> {};

template <typename Tr, TryTupleTraversableBy<Tr> T>
struct TraversableTrait<Tr, T>
    : TraversableTupleTrait<
          Tr, T, std::make_index_sequence<std::tuple_size<T>::value>> {};

template <typename Tr, TryContainerTraversableBy<Tr> T>
struct TraversableTrait<Tr, T> : TraversableTrait<Tr, typename T::value_type> {
};

template <typename T, typename F, size_t... Ns>
void TupleLikeApply(std::index_sequence<Ns...>, F&& f, T&& t) {
  using std::get;
  std::forward<F>(f)(get<Ns>(std::forward<T>(t))...);
}

}  // namespace internal_traverse

template <typename T, typename Tr>
concept TraversableBy = ::base::internal_traverse::TraversableTrait<Tr, T>::value;

template <typename Tr>
void Traverse(Tr& t, TraversableBy<Tr> auto&... values);

namespace internal_traverse {

template <typename Tr, TraversableBy<Tr> T>
void TraverseOne(Tr& t, T& value) {
  if constexpr (requires { t(value); }) {
    t(value);
  } else if constexpr (requires { BaseTraverse(t, value); }) {
    BaseTraverse(t, value);
  } else if constexpr (::base::SatisfiesTupleProtocol<T>) {
    TupleLikeApply(std::make_index_sequence<std::tuple_size<T>::value>{},
                   [&](auto&... values) { ::base::Traverse(t, values...); },
                   value);
  } else if constexpr (requires {
                         typename T::value_type;
                         ++value.begin() == value.end();
                       } and
                       TraversableBy<typename T::value_type, Tr>) {
    for (auto& element : value) { ::base::Traverse(t, element); }
  } else {
    static_assert(base::always_false<T>());
  }
}

}  // namespace internal_traverse

template <typename Tr>
void Traverse(Tr& t, TraversableBy<Tr> auto&... values) {
  (::base::internal_traverse::TraverseOne(t, values), ...);
}

}  // namespace base

#endif  // ICARUS_BASE_TRAVERSE_H
