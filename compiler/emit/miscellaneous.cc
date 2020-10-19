#include "ast/ast.h"
#include "compiler/compiler.h"
#include "ir/value/module_id.h"
#include "ir/value/value.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::ArgumentType const *node) {
  return ir::Value(data().arg_type(node->name()));
}

ir::Value Compiler::EmitValue(ast::BuiltinFn const *node) {
  return ir::Value(ir::Fn(node->value()));
}

ir::Value Compiler::EmitValue(ast::Import const *node) {
  auto module_id = data().imported_module(node);
  ASSERT(module_id != ir::ModuleId::Invalid());
  return ir::Value(module_id);
}

ir::Value Compiler::EmitValue(ast::Label const *node) {
  return ir::Value(node->value());
}

}  // namespace compiler
