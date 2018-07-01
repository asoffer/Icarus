#ifndef ICARUS_BASE_STALE_SET_H
#define ICARUS_BASE_STALE_SET_H

#include <unordered_set>
namespace base {
template <typename T>
struct stale_set {
 public:
  template <typename... Args>
  stale_set(Args&&... args) : stale_entries_(std::forward<Args>(args)...) {}

  template <typename Fn>
  void until_empty(Fn&& fn) {
    while (!stale_entries_.empty()) {
      auto iter = stale_entries_.begin();
      while (iter != stale_entries_.end()) {
        auto next_iter = std::next(iter);
        fn(stale_entries_.extract(iter).value());
        iter = next_iter;
      }
    }
  }

  template <typename... Args>
  void emplace(Args&&... args) {
    stale_entries_.emplace(std::forward<Args>(args)...);
  }
  void insert(T&& val) { stale_entries_.insert(std::move(val)); }
  void insert(const T& val) { stale_entries_.insert(val); }

 private:
  std::unordered_set<T> stale_entries_;
};
}  // namespace base

#endif  // ICARUS_BASE_STALE_SET_H
