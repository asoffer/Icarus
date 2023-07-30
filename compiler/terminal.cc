#include "ast/ast.h"
#include "compiler/compiler.h"
#include "core/type_system/type.h"
#include "absl/numeric/int128.h"

namespace compiler {

#define ICARUS_HANDLE_TERMINAL_CASE(t, T)                                      \
  ICARUS_HANDLE_TERMINAL_CASE_WITH_ACTION(t, T, AppendPush(node->value<t>()))

#define ICARUS_HANDLE_TERMINAL_CASE_WITH_ACTION(t, T, action)                  \
  if (node->type() == nth::type<t>) {                                          \
    co_yield std::vector{Constant(T)};                                         \
    auto data = co_await nth::type<FunctionData>;                              \
    LOG("", "%v", data.kind());                                                \
    switch (data.kind()) {                                                     \
      case EmitKind::Value: data.function().action; [[fallthrough]];           \
      case EmitKind::Statement: co_return;                                     \
      default: UNREACHABLE();                                                  \
    }                                                                          \
  }

Compiler::Task Compiler::TaskFor(ast::Terminal const* node) {
  ICARUS_HANDLE_TERMINAL_CASE(bool, Bool);
  ICARUS_HANDLE_TERMINAL_CASE(data_types::Char, Char);
  ICARUS_HANDLE_TERMINAL_CASE(core::Type, Type);
  ICARUS_HANDLE_TERMINAL_CASE_WITH_ACTION(
      absl::int128, Integer,
      AppendPush(module().integer_table().insert(node->value<absl::int128>())));
  ICARUS_HANDLE_TERMINAL_CASE(double, F64);
  ICARUS_HANDLE_TERMINAL_CASE(data_types::addr_t, NullPtr);
  ICARUS_HANDLE_TERMINAL_CASE_WITH_ACTION(
      std::string, SliceType(type_system(), Char),
      AppendPushStringLiteral(node->value<std::string>()));
  NOT_YET(node->DebugString());
}

#undef ICARUS_HANDLE_TERMINAL_CASE

}  // namespace compiler
