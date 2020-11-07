#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {

void Compiler::EmitMoveInit(
    ast::Identifier const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  EmitMoveInit(type::Typed<ir::Value>(EmitValue(node),
                                      context().qual_type(node)->type()),
               type::Typed<ir::Reg>(to[0]->reg(), to[0].type()));
}

void Compiler::EmitCopyInit(
    ast::Identifier const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  EmitCopyInit(type::Typed<ir::Value>(EmitValue(node),
                                      context().qual_type(node)->type()),
               type::Typed<ir::Reg>(to[0]->reg(), to[0].type()));
}

ir::Value Compiler::EmitValue(ast::Identifier const *node) {
  LOG("Identifier", "%s", node->name());
  auto decl_span = context().decls(node);
  ASSERT(decl_span.size() != 0u);
  if (decl_span[0]->flags() & ast::Declaration::f_IsConst) {
    return EmitValue(decl_span[0]);
  }
  if (decl_span[0]->flags() & ast::Declaration::f_IsFnParam) {
    auto t      = type_of(node);
    ir::Reg reg = context().addr(decl_span[0]);
    return (decl_span[0]->flags() & ast::Declaration::f_IsOutput) and
                   not t.get()->is_big()
               ? builder().Load(reg, t)
               : ir::Value(reg);
  } else {
    type::Type t = type_of(node);
    auto lval    = EmitRef(node);
    if (not lval.is_reg()) { NOT_YET(); }
    return ir::Value(builder().PtrFix(lval.reg(), t));
  }
}

void Compiler::EmitAssign(
    ast::Identifier const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_type(node)->type();
  EmitCopyAssign(to[0], type::Typed<ir::Value>(EmitValue(node), t));
}

ir::RegOr<ir::Addr> Compiler::EmitRef(ast::Identifier const *node) {
  auto decl_span = context().decls(node);
  ASSERT(decl_span.size() == 1u);
  return context().addr(decl_span[0]);
}

}  // namespace compiler
