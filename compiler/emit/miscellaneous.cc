#include "ast/ast.h"
#include "compiler/compiler.h"
#include "ir/value/module_id.h"
#include "ir/value/value.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::ArgumentType const *node,
                         base::untyped_buffer &out) {
  out.append(ir::RegOr<type::Type>(context().arg_type(node->name())));
}

void Compiler::EmitToBuffer(ast::BuiltinFn const *node,
                         base::untyped_buffer &out) {
  out.append(ir::RegOr<ir::Fn>(ir::Fn(node->value())));
}

void Compiler::EmitToBuffer(ast::Import const *node, base::untyped_buffer &out) {
  auto module_id = context().imported_module(node);
  ASSERT(module_id != ir::ModuleId::Invalid());
  out.append(ir::RegOr<ir::ModuleId>(module_id));
}

void Compiler::EmitToBuffer(ast::Label const *node, base::untyped_buffer &out) {
  out.append(ir::RegOr<ir::Label>(node->value()));
}

}  // namespace compiler
