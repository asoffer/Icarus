#include "ast/ast.h"
#include "compiler/compiler.h"
#include "ir/value/module_id.h"
#include "ir/value/value.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::ArgumentType const *node,
                            ir::PartialResultBuffer &out) {
  out.append(context().arg_type(node->name()));
}

void Compiler::EmitToBuffer(ast::BuiltinFn const *node,
                            ir::PartialResultBuffer &out) {
  out.append(ir::Fn(node->value()));
}

void Compiler::EmitToBuffer(ast::Import const *node,
                            ir::PartialResultBuffer &out) {
  auto module_id = context().imported_module(node);
  ASSERT(module_id != ir::ModuleId::Invalid());
  out.append(module_id);
}

void Compiler::EmitToBuffer(ast::Label const *node,
                            ir::PartialResultBuffer &out) {
  out.append(node->value());
}

}  // namespace compiler
