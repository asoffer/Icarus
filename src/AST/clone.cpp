#include "AST.h"
#include "Context.h"
#include "DependencySystem.h"

namespace AST {
StructLiteral *StructLiteral::clone(Context &ctx) {
  auto struct_lit = new StructLiteral;
  struct_lit->declarations.reserve(declarations.size());
  for (auto decl: declarations) {
    auto type_expr_ptr        = decl->type_expr->evaluate(ctx).as_type;

    auto new_decl        = new Declaration;
    new_decl->identifier = new Identifier(line_num, decl->identifier->token());
    new_decl->line_num   = decl->line_num;
    new_decl->decl_type  = decl->decl_type;

    new_decl->type_expr = new DummyTypeExpr(decl->line_num, type_expr_ptr);


    Scope::Stack.push(type_scope);
    new_decl->assign_scope();
    Scope::Stack.pop();

    // no need to do type verification
    new_decl->type = type_expr_ptr;
    type_expr_ptr->generate_llvm();

    struct_lit->declarations.push_back(new_decl);
  }

  // we need to generate it's dependencies.
  Dependency::mark_as_done(struct_lit);

  return struct_lit;
}
} // namespace AST
