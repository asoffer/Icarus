#ifndef ICARUS_BASE_SCOPE_H
#define ICARUS_BASE_SCOPE_H

#include <type_traits>

// Constructs an object passed into the macro and destroys the object when the
// scope ends. This is intended to be used with RAII-like objects to avoid the
// necessity of providing a name for the objects.
//
// Example:
// ```
// ICARUS_SCOPE(std::lock_guard(mtx)) {
//   ...
// }
// ```
#define ICARUS_SCOPE(...)                                                      \
  static_assert(std::is_base_of_v<::base::UseWithScope,                        \
                                  std::decay_t<decltype(__VA_ARGS__)>>,        \
                "If a type is to be used with ICARUS_SCOPE, it must opt-in "   \
                "by publicly extending base::UseWithScope");                   \
  if (auto ICARUS_CAT(icarus_temp_scope_, __LINE__, _var) = __VA_ARGS__; true)

namespace base {

struct UseWithScope {};

}  // namespace base

#endif  // ICARUS_BASE_SCOPE_H
