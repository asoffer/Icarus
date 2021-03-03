#include "ast/ast.h"
#include "compiler/compiler.h"
#include "ir/value/addr.h"
#include "ir/value/reg_or.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/typed_value.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::InterfaceLiteral const *node) {
  return ir::Value();
}

}  // namespace compiler
