#include "ast/ast.h"
#include "compiler/compiler.h"
#include "core/type_system/type.h"
#include "nth/numeric/integer.h"

namespace compiler {

Compiler::Task Compiler::TaskFor(ast::Terminal const* node) {
  if (node->type() == nth::type<bool>) {
    co_yield std::vector{Constant(Bool)};
    auto data = co_await nth::type<FunctionData>;
    data.function().AppendPush(node->value<bool>());
  } else if (node->type() == nth::type<data_types::Char>) {
    co_yield std::vector{Constant(Char)};
    auto data = co_await nth::type<FunctionData>;
    data.function().AppendPush(node->value<data_types::Char>());
  } else if (node->type() == nth::type<core::Type>) {
    co_yield std::vector{Constant(Type)};
    auto data = co_await nth::type<FunctionData>;
    data.function().AppendPush(node->value<core::Type>());
  } else if (node->type() == nth::type<nth::Integer>) {
    co_yield std::vector{Constant(Integer)};
    auto data = co_await nth::type<FunctionData>;
    data.function().AppendPush(
        module().integer_table().insert(node->value<nth::Integer>()));
  } else if (node->type() == nth::type<double>) {
    co_yield std::vector{Constant(F64)};
    auto data = co_await nth::type<FunctionData>;
    data.function().AppendPush(node->value<double>());
  } else if (node->type() == nth::type<data_types::addr_t>) {
    co_yield std::vector{Constant(NullPtr)};
    auto data = co_await nth::type<FunctionData>;
    data.function().AppendPush(data_types::Null());
  } else if (node->type() == nth::type<std::string>) {
    co_yield std::vector{Constant(SliceType(type_system(), Char))};
    auto data = co_await nth::type<FunctionData>;
    data.function().AppendPushStringLiteral(node->value<std::string>());
  } else {
    NOT_YET(node->DebugString());
  }
}

}  // namespace compiler
