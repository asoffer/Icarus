#ifndef ICARUS_BASE_CONTAINER_MAP_H
#define ICARUS_BASE_CONTAINER_MAP_H

#include <cstdlib>
#include <map>

namespace base {
#ifdef DBG
template <typename... Ts>
struct map : public std::map<Ts...> {
 private:
  using MapT = typename std::map<Ts...>;
  using key_type = typename MapT::key_type;
  using mapped_type = typename MapT::mapped_type;

 public:
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
template <typename... Ts>
using map = std::map<Ts...>;
#endif
}  // namespace base

#endif  // ICARUS_BASE_CONTAINER_MAP_H
