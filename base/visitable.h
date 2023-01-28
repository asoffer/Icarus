#ifndef ICARUS_BASE_VISITOR_H
#define ICARUS_BASE_VISITOR_H

#include <array>
#include <utility>

#include "nth/meta/sequence.h"
#include "nth/meta/type.h"

namespace base {

template <typename T, auto Nodes>
struct Visitable {
  constexpr explicit Visitable(int8_t which) : which_(which) {}

  template <typename V, typename... Args>
  auto visit(V &v, Args &&...args) const {
    return vtable<V>(Nodes,
                     static_cast<typename V::signature *>(nullptr))[which_](
        static_cast<T const *>(this), v, std::forward<Args>(args)...);
  }

 protected:
  int8_t which() const { return which_; }

  int8_t which_;

 private:
  template <typename V, typename Ret, typename... Args>
  auto const &vtable(nth::Sequence auto types, Ret (*)(Args...)) const {
    static constexpr std::array vtable_ = types.reduce([](auto... ts) {
      return std::array{+[](void const *p, V &v, Args... args) -> Ret {
        return v(reinterpret_cast<nth::type_t<decltype(ts){}> const *>(p),
                 std::forward<Args>(args)...);
      }...};
    });
    return vtable_;
  }
};

}  // namespace base

#endif  // ICARUS_BASE_VISITOR_H
