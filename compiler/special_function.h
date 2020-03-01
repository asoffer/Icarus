#ifndef ICARUS_COMPILER_SPECIAL_FUNCTION
#define ICARUS_COMPILER_SPECIAL_FUNCTION

#include <optional>

#include "compiler/compiler.h"
#include "ir/value/fn.h"
#include "type/struct.h"

namespace compiler {

enum SpecialFunctionCategory { Copy, Move };

template <SpecialFunctionCategory Cat>
constexpr char const *Name() {
  if constexpr (Cat == Move) { return "move"; }
  if constexpr (Cat == Copy) { return "copy"; }
}

std::optional<ir::Fn> SpecialFunction(Compiler *compiler, type::Struct const *s,
                                      char const *symbol);

}  // namespace compiler
#endif  // ICARUS_COMPILER_SPECIAL_FUNCTION
