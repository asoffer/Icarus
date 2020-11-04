#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {
namespace {

std::optional<type::Quals> VerifyAndGetQuals(
    Compiler *v, base::PtrSpan<ast::Expression const> exprs) {
  bool err          = false;
  type::Quals quals = type::Quals::All();
  for (auto *expr : exprs) {
    auto r = v->VerifyType(expr);
    err |= not r.ok();
    if (not err) { quals &= r.quals(); }
  }
  if (err) { return std::nullopt; }
  return quals;
}

}  // namespace

type::QualType Compiler::VerifyType(ast::ArgumentType const *node) {
  return context().set_qual_type(node, type::QualType::Constant(type::Type_));
}

type::QualType Compiler::VerifyType(ast::BuiltinFn const *node) {
  return context().set_qual_type(
      node, type::QualType::Constant(node->value().type()));
}

type::QualType Compiler::VerifyType(ast::ReturnStmt const *node) {
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto quals, VerifyAndGetQuals(this, node->exprs()));
  return type::QualType(type::Void(), quals);
}

type::QualType Compiler::VerifyType(ast::YieldStmt const *node) {
  ASSIGN_OR(return type::QualType::Error(),  //
                   auto quals, VerifyAndGetQuals(this, node->exprs()));
  return type::QualType(type::Void(), quals);
}

type::QualType Compiler::VerifyType(ast::BlockNode const *node) {
  for (auto &param : node->params()) { VerifyType(param.value.get()); }
  for (auto *stmt : node->stmts()) { VerifyType(stmt); }
  // TODO: Determine the type of the block (i.e., what type might be yielded out
  // of it?
  return context().set_qual_type(node, type::QualType::Constant(type::Block));
}

type::QualType Compiler::VerifyType(ast::ScopeNode const *node) {
  LOG("ScopeNode", "Verifying ScopeNode named `%s` at %d:%d",
      node->name()->DebugString(), node->name()->range().begin().line_num.value,
      node->name()->range().begin().offset.value);
  // TODO: The type of the arguments and the scope name are independent and
  // should not have early-exists.

  ASSIGN_OR(return type::QualType::Error(),  //
                   std::ignore, VerifyArguments(node->args()));

  ASSIGN_OR(return type::QualType::Error(),  //
                   std::ignore, VerifyType(node->name()));
  for (auto const &block : node->blocks()) { VerifyType(&block); }
  // TODO hack. Set this for real.
  return context().set_qual_type(node,
                                 type::QualType::NonConstant(type::Void()));
}

type::QualType Compiler::VerifyType(ast::Label const *node) {
  return context().set_qual_type(node, type::QualType::Constant(type::Label));
}

}  // namespace compiler
