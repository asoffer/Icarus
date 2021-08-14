#ifndef ICARUS_COMPILER_INSTANTIATE_H
#define ICARUS_COMPILER_INSTANTIATE_H

#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/context.h"
#include "core/arguments.h"
#include "ir/value/result_buffer.h"
#include "type/typed_value.h"

namespace compiler {

struct TypedConstants {
  ir::CompleteResultBuffer constants;
  core::Arguments<type::Typed<ir::CompleteResultRef>> arguments;
};

// Attemnts to instantiate `node` with `args`, possibly creating a new
// instantiation as a subcontext of `this->context()` if needed.
Context::InsertSubcontextResult Instantiate(
    Compiler &c, ast::ParameterizedExpression const *node,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &args);

// Finds an already existing instantiation of `node` with `args` as a
// subcontext of `this->context()`. Behavior is undefined if none exists.
Context::FindSubcontextResult FindInstantiation(
    Compiler &c, ast::ParameterizedExpression const *node,
    core::Arguments<type::Typed<ir::CompleteResultRef>> const &args);

}  // namespace compiler

#endif  // ICARUS_COMPILER_INSTANTIATE_H
