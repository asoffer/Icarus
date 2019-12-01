#ifndef ICARUS_COMPILER_DISPATCH_OVERLOAD_H
#define ICARUS_COMPILER_DISPATCH_OVERLOAD_H

#include "ast/ast.h"
#include "core/fn_params.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {
namespace internal {

// TODO rename to `Overload` or sometihng similar
struct ExprData {
  type::Type const *type;
  core::FnParams<type::Typed<ast::Declaration const *>> params;
};

}  // namespace internal
}  // namespace compiler

#endif  // ICARUS_COMPILER_DISPATCH_OVERLOAD_H
