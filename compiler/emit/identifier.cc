#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::Identifier const *node) {
  auto potential_decls = data().decls(node);
  switch (potential_decls.size()) {
    case 0:
      UNREACHABLE(
          "If this decl didn't exist type-checking for this identifier would "
          "have already triggered.");
    case 1: {
      auto const *decl = potential_decls[0];
      if (decl->flags() & ast::Declaration::f_IsConst) {
        return EmitValue(decl);
      } else if (decl->flags() & ast::Declaration::f_IsFnParam) {
        auto const &qt = *ASSERT_NOT_NULL(data().qual_type(node));
        ir::Reg reg    = data().addr(decl);
        return (decl->flags() & ast::Declaration::f_IsOutput) and
                       not qt.type()->is_big()
                   ? builder().Load(reg, qt.type())
                   : ir::Value(reg);
      } else {
        auto const &qt = *ASSERT_NOT_NULL(data().qual_type(node));
        auto lval      = EmitRef(node);
        if (not lval.is_reg()) { NOT_YET(); }
        return ir::Value(builder().PtrFix(lval.reg(), qt.type()));
      }
    } break;
    default: NOT_YET();
  }
}

ir::RegOr<ir::Addr> Compiler::EmitRef(ast::Identifier const *node) {
  auto potential_decls = data().decls(node);
  ASSERT(potential_decls.size() == 1u);
  return data().addr(potential_decls[0]);
}

// TODO: Unit tests
void Compiler::EmitMoveInit(
    ast::Identifier const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  EmitMoveInit(
      type::Typed<ir::Value>(EmitValue(node), data().qual_type(node)->type()),
      type::Typed<ir::Reg>(to[0]->reg(), to[0].type()));
}

void Compiler::EmitCopyInit(
    ast::Identifier const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  EmitCopyInit(
      type::Typed<ir::Value>(EmitValue(node), data().qual_type(node)->type()),
      type::Typed<ir::Reg>(to[0]->reg(), to[0].type()));
}

void Compiler::EmitAssign(
    ast::Identifier const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = data().qual_type(node)->type();
  Visit(t, *to[0], type::Typed{EmitValue(node), t}, EmitCopyAssignTag{});
}

}  // namespace compiler
