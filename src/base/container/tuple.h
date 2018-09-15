#ifndef ICARUS_BASE_CONTAINER_TUPLE_H
#define ICARUS_BASE_CONTAINER_TUPLE_H

#include <tuple>
#include <type_traits>
#include <utility>

namespace base {
namespace tuple {
namespace internal {
template <typename Seq>
struct with_seq;

template <size_t N, typename Fn, typename... Tups>
constexpr auto call_at_index(Fn &&fn, Tups &&... tups) {
  return fn(std::get<N>(tups)...);
}

template <size_t... Ns>
struct with_seq<std::index_sequence<Ns...>> {
  template <typename Fn, typename... Tups>
  constexpr static auto transform(Fn &&fn, Tups &&... tups) {
    return std::make_tuple(
        ::base::tuple::internal::call_at_index<Ns>(fn, tups...)...);
  }

  template <typename Fn, typename... Tups>
  constexpr static void for_each(Fn &&fn, Tups &&... tups) {
    (::base::tuple::internal::call_at_index<Ns>(fn, tups...), ...);
  }
};
}  // namespace internal

template <typename Fn, typename... Tups>
auto transform(Fn &&fn, Tups &&... tups) {
  constexpr auto tup_size = std::tuple_size_v<
      std::tuple_element_t<0, std::tuple<std::decay_t<Tups>...>>>;
  return ::base::tuple::internal::with_seq<
      std::make_index_sequence<tup_size>>::transform(std::forward<Fn>(fn),
                                                     std::forward<Tups>(
                                                         tups)...);
}

template <typename Fn, typename... Tups>
void for_each(Fn &&fn, Tups &&... tups) {
  constexpr auto tup_size =
      std::tuple_size_v<std::tuple_element_t<0, std::tuple<Tups...>>>;
  return ::base::tuple::internal::with_seq<
      std::make_index_sequence<tup_size>>::for_each(std::forward<Fn>(fn),
                                                    std::forward<Tups>(
                                                        tups)...);
}
}  // namespace tuple
}  // namespace base

#endif  // ICARUS_BASE_CONTAINER_TUPLE_H
