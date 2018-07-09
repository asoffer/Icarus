#ifndef ICARUS_BASE_CONTAINER_UNORDERED_MAP_H
#define ICARUS_BASE_CONTAINER_UNORDERED_MAP_H

#include <cstdlib>
#include <unordered_map>

namespace base {
#ifdef DBG
template <typename... Ts>
struct unordered_map : public std::unordered_map<Ts...> {
 private:
  using MapT        = typename std::unordered_map<Ts...>;
  using key_type    = typename MapT::key_type;
  using mapped_type = typename MapT::mapped_type;
  using value_type  = typename MapT::value_type;

 public:
  template <typename... Args>
  unordered_map(Args&&... args) : MapT(std::forward<Args>(args)...) {}
  unordered_map(std::initializer_list<value_type> init)
      : MapT(std::forward<std::initializer_list<value_type>>(init)) {}

  mapped_type& at(const key_type& key) {
    auto iter = this->find(key);
    if (iter == this->end()) std::abort();
    return MapT::at(key);
  }

  const mapped_type& at(const key_type& key) const {
    auto iter = this->find(key);
    if (iter == this->end()) std::abort();
    return MapT::at(key);
  }
};
#else
using unordered_map = std::unordered_map;
#endif
}  // namespace base

#endif  // ICARUS_BASE_CONTAINER_UNORDERED_MAP_H
