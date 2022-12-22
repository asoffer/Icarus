#ifndef ICARUS_BASE_VISITOR_H
#define ICARUS_BASE_VISITOR_H

#include <array>
#include <utility>

#include "base/meta.h"

namespace base {

template <typename T, typename Nodes>
struct Visitable {
  constexpr explicit Visitable(int8_t which) : which_(which) {}

  template <typename V, typename... Args>
  auto visit(V &v, Args &&...args) const {
    return vtable<V>(Nodes{},
                     static_cast<typename V::signature *>(nullptr))[which_](
        static_cast<T const *>(this), v, std::forward<Args>(args)...);
  }

 protected:
  int8_t which() const { return which_; }

  int8_t which_;

 private:
  template <typename V, typename... Ts, typename Ret, typename... Args>
  auto const &vtable(type_list<Ts...>, Ret (*)(Args...)) const {
    static constexpr std::array vtable_{+[](void const *p, V &v,
                                            Args... args) -> Ret {
      return v(reinterpret_cast<Ts const *>(p), std::forward<Args>(args)...);
    }...};
    return vtable_;
  }
};

}  // namespace base

#endif  // ICARUS_BASE_VISITOR_H
