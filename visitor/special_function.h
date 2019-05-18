#ifndef ICARUS_VISITOR_SPECIAL_FUNCTION
#define ICARUS_VISITOR_SPECIAL_FUNCTION

#include <optional>

struct Context;

namespace type {
struct Struct;
}  // namespace type

namespace ir {
struct AnyFunc;
}  // namespace ir

namespace visitor {
struct EmitIr;

enum SpecialFunctionCategory { Copy, Move };

template <SpecialFunctionCategory Cat>
constexpr char const *Name() {
  if constexpr (Cat == Move) { return "move"; }
  if constexpr (Cat == Copy) { return "copy"; }
}

std::optional<ir::AnyFunc> SpecialFunction(EmitIr const *visitor,
                                           type::Struct const *s,
                                           char const *symbol, Context *ctx);

}  // namespace visitor
#endif  // ICARUS_VISITOR_SPECIAL_FUNCTION
