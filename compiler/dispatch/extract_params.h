#ifndef ICARUS_COMPILER_DISPATCH_EXTRACT_PARAMS_H
#define ICARUS_COMPILER_DISPATCH_EXTRACT_PARAMS_H

#include "ast/ast_fwd.h"
#include "compiler/compiler.h"
#include "core/fn_params.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

core::FnParams<type::Typed<ast::Declaration const *>> ExtractParams(
    Compiler *compiler, type::Typed<ast::Expression const *> expr);

}  // namespace compiler

#endif  // ICARUS_COMPILER_DISPATCH_EXTRACT_PARAMS_H
