#include "ast/scope_literal.h"

#include "ast/declaration.h"
#include "ast/verify_macros.h"
#include "error/log.h"
#include "ir/val.h"
#include "scope.h"
#include "type/function.h"
#include "type/scope.h"

namespace AST {
std::string ScopeLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << "scope {\n";
  for (const auto &decl : decls_) {
    ss << std::string(n * 2, ' ') << decl.to_string(n) << "\n";
  }
  ss << "}";
  return ss.str();
}

void ScopeLiteral::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_     = scope;
  body_scope_ = scope->add_child<DeclScope>();
  for (auto &decl : decls_) { decl.assign_scope(body_scope_.get()); }
}

void ScopeLiteral::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  type = type::Scp({});
  ctx->types_.buffered_emplace(this, type::Scp({}));
  // TODO
}

void ScopeLiteral::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  for (auto &decl : decls_) {
    decl.VerifyType(ctx);
    decl.Validate(ctx);
  }
}

void ScopeLiteral::SaveReferences(Scope *scope, base::vector<IR::Val> *args) {
  for (auto &decl : decls_) { decl.SaveReferences(scope, args); }
}

void ScopeLiteral::contextualize(
    const Node *correspondant,
    const base::unordered_map<const Expression *, IR::Val> &replacements) {
  for (size_t i = 0; i < decls_.size(); ++i) {
    decls_[i].contextualize(&correspondant->as<ScopeLiteral>().decls_[i],
                            replacements);
  }
}

void ScopeLiteral::ExtractReturns(base::vector<const Expression *> *rets) const {
  for (auto &decl : decls_) { decl.ExtractReturns(rets); }
}

ScopeLiteral *ScopeLiteral::Clone() const {
  auto *result = new ScopeLiteral;
  result->span = span;
  result->decls_.reserve(decls_.size());
  for (const auto &decl : decls_) {
    result->decls_.emplace_back(decl.Clone());
  }
  return result;
}

base::vector<IR::Val> AST::ScopeLiteral::EmitIR(Context *ctx) {
  return {IR::Val(this)};
}
base::vector<IR::Register> ScopeLiteral::EmitLVal(Context *) { UNREACHABLE(this); }
}  // namespace AST
