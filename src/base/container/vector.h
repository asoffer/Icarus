#ifndef ICARUS_BASE_CONTAINER_VECTOR_H
#define ICARUS_BASE_CONTAINER_VECTOR_H

#include <cstdlib>
#include <vector>

namespace base {
#ifdef DBG
template <typename... Ts>
struct vector : public std::vector<Ts...> {
 private:
  using VecT       = typename std::vector<Ts...>;
  using value_type = typename VecT::value_type;

 public:
  template <typename... Args>
  vector(Args&&... args) : VecT(std::forward<Args>(args)...) {}
  vector(std::initializer_list<value_type> init)
      : VecT(std::forward<std::initializer_list<value_type>>(init)) {}

  typename VecT::reference at(size_t pos) {
    if (pos >= this->size()) std::abort();
    return VecT::at(pos);
  }

  typename VecT::const_reference at(size_t pos) const {
    if (pos >= this->size()) std::abort();
    return VecT::at(pos);
  }
};

#else
template <typename... Ts>
using vector = std::vector<Ts...>;
#endif
}  // namespace base

#endif  // ICARUS_BASE_CONTAINER_VECTOR_H
