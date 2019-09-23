#ifndef ICARUS_VISITOR_SPECIAL_FUNCTION
#define ICARUS_VISITOR_SPECIAL_FUNCTION

#include <optional>

#include "ir/any_func.h"

namespace type {
struct Struct;
}  // namespace type

namespace visitor {
struct TraditionalCompilation;

enum SpecialFunctionCategory { Copy, Move };

template <SpecialFunctionCategory Cat>
constexpr char const *Name() {
  if constexpr (Cat == Move) { return "move"; }
  if constexpr (Cat == Copy) { return "copy"; }
}

std::optional<ir::AnyFunc> SpecialFunction(TraditionalCompilation *visitor,
                                           type::Struct const *s,
                                           char const *symbol);

}  // namespace visitor
#endif  // ICARUS_VISITOR_SPECIAL_FUNCTION
