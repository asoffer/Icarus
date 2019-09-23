#ifndef ICARUS_COMPILER_SPECIAL_FUNCTION
#define ICARUS_COMPILER_SPECIAL_FUNCTION

#include <optional>

#include "ir/any_func.h"

namespace type {
struct Struct;
}  // namespace type

namespace compiler {
struct Compiler;

enum SpecialFunctionCategory { Copy, Move };

template <SpecialFunctionCategory Cat>
constexpr char const *Name() {
  if constexpr (Cat == Move) { return "move"; }
  if constexpr (Cat == Copy) { return "copy"; }
}

std::optional<ir::AnyFunc> SpecialFunction(Compiler *compiler,
                                           type::Struct const *s,
                                           char const *symbol);

}  // namespace compiler
#endif  // ICARUS_COMPILER_SPECIAL_FUNCTION
