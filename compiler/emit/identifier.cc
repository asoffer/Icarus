#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {

void Compiler::EmitMoveInit(
    ast::Identifier const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  EmitMoveInit(type::Typed<ir::Reg>(to[0]->reg(), to[0].type()),
               type::Typed<ir::Value>(EmitValue(node),
                                      context().qual_types(node)[0].type()));
}

void Compiler::EmitCopyInit(
    ast::Identifier const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  EmitCopyInit(type::Typed<ir::Reg>(to[0]->reg(), to[0].type()),
               type::Typed<ir::Value>(EmitValue(node),
                                      context().qual_types(node)[0].type()));
}

ir::Value Compiler::EmitValue(ast::Identifier const *node) {
  LOG("Identifier", "%s", node->name());
  auto decl_span = context().decls(node);
  ASSERT(decl_span.size() != 0u);
  if (decl_span[0]->flags() & ast::Declaration::f_IsConst) {
    auto const *mod = &decl_span[0]
                           ->scope()
                           ->Containing<ast::ModuleScope>()
                           ->module()
                           ->as<CompiledModule>();
    if (mod != &context().module()) {
      return mod->context().Constant(&decl_span[0]->ids()[0])->value();
    } else {
      return EmitValue(decl_span[0]);
    }
  }
  if (decl_span[0]->flags() & ast::Declaration::f_IsFnParam) {
    auto t = context().qual_types(node)[0].type();
    // TODO: Support multiple declarations
    ir::Reg reg = context().addr(&decl_span[0]->ids()[0]);
    return (decl_span[0]->flags() & (ast::Declaration::f_IsBlockParam |
                                     ast::Declaration::f_IsOutput)) and
                   not t.get()->is_big()
               ? builder().Load(reg, t)
               : ir::Value(reg);
  } else {
    type::Type t = context().qual_types(node)[0].type();
    auto lval    = EmitRef(node);
    return ir::Value(builder().PtrFix(lval, t));
  }
}

void Compiler::EmitCopyAssign(
    ast::Identifier const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  EmitCopyAssign(to[0], type::Typed<ir::Value>(EmitValue(node), t));
}

void Compiler::EmitMoveAssign(
    ast::Identifier const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  EmitMoveAssign(to[0], type::Typed<ir::Value>(EmitValue(node), t));
}

ir::Reg Compiler::EmitRef(ast::Identifier const *node) {
  auto decl_span = context().decls(node);
  ASSERT(decl_span.size() == 1u);
  for (auto const &id : decl_span[0]->ids()) {
    if (id.name() != node->name()) { continue; }
    return context().addr(&id);
  }
  UNREACHABLE();
}

}  // namespace compiler
