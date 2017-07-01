#ifndef ICARUS_BASE_CAST_TOOLS_H
#define ICARUS_BASE_CAST_TOOLS_H

#include <type_traits>

namespace base {
template <typename Base> struct Cast {
  template <typename T> bool is() const {
    static_assert(std::is_base_of<Base, T>::value,
                  "Calling is<...> but there is no inheritance relationship. "
                  "Result is vacuously false.");
    return dynamic_cast<const T *>(reinterpret_cast<const Base *>(this)) !=
           nullptr;
  }
};
} // namespace base

#endif // ICARUS_BASE_CAST_TOOLS_H
