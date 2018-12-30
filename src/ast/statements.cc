#include "ast/statements.h"

#include "ir/val.h"

namespace ast {
std::string Statements::to_string(size_t n) const {
  if (content_.empty()) { return ""; }

  std::stringstream ss;
  for (const auto &stmt : content_) {
    ss << std::string(n * 2, ' ') << stmt->to_string(n) << "\n";
  }
  return ss.str();
}

void Statements::assign_scope(Scope *scope) {
  scope_ = scope;
  for (auto &stmt : content_) { stmt->assign_scope(scope); }
}

type::Type const *Statements::VerifyType(Context *ctx) {
  for (auto &stmt : content_) { stmt->VerifyType(ctx); }
  return nullptr;
}

void Statements::Validate(Context *ctx) {
  for (auto &stmt : content_) { stmt->Validate(ctx); }
}

void Statements::ExtractJumps(JumpExprs *rets) const {
  for (auto &stmt : content_) { stmt->ExtractJumps(rets); }
}

base::vector<ir::Val> ast::Statements::EmitIR(Context *ctx) {
  for (auto &stmt : content_) { stmt->EmitIR(ctx); }
  return {};
}

}  // namespace ast
