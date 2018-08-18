#ifndef ICARUS_BASE_CONTAINER_STALE_H
#define ICARUS_BASE_CONTAINER_STALE_H

#include <unordered_map>
#include <unordered_set>

namespace base {
template <typename... Ts>
struct stale_set : public std::unordered_set<Ts...> {
  template <typename Fn>
  void until_empty(Fn&& fn) {
    while (!this->empty()) {
      auto iter = this->begin();
      while (iter != this->end()) {
        auto next_iter = std::next(iter);
        fn(this->extract(iter).value());
        iter = next_iter;
      }
    }
  }
};

template <typename... Ts>
struct stale_map : public std::unordered_map<Ts...> {
  template <typename Fn>
  void until_empty(Fn&& fn) {
    while (!this->empty()) {
      auto iter = this->begin();
      while (iter != this->end()) {
        auto next_iter = std::next(iter);
        auto node      = this->extract(iter);
        fn(node.key(), node.mapped());
        iter = next_iter;
      }
    }
  }
};
}  // namespace base

#endif  // ICARUS_BASE_CONTAINER_STALE_H
