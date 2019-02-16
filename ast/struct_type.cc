#include "ast/struct_type.h"

#include <sstream>
#include "misc/context.h"
#include "type/type.h"

namespace ast {
std::string StructType::to_string(size_t n) const {
  if (args_.empty()) { return "[; struct]"; }
  std::stringstream ss;
  auto iter = args_.begin();
  ss << "[" << (**iter++).to_string(n);
  for (; iter != args_.end(); ++iter) { ss << ", " << (**iter).to_string(n); }
  ss << "; struct]";
  return ss.str();
}

void StructType::assign_scope(Scope *scope) {
  for (auto &arg : args_) { arg->assign_scope(scope); }
}

void StructType::DependentDecls(base::Graph<Declaration *> *g,
                                Declaration *d) const {
  for (auto &arg : args_) { arg->DependentDecls(g, d); }
}

VerifyResult StructType::VerifyType(Context *ctx) {
  for (auto &arg : args_) { arg->VerifyType(ctx); }
  return VerifyResult::Constant(ctx->set_type(this, type::Type_));
}

std::vector<ir::Val> StructType::EmitIR(Context *ctx) { NOT_YET(); }

void StructType::ExtractJumps(JumpExprs *rets) const {
  for (auto &arg : args_) { arg->ExtractJumps(rets); }
}
}  // namespace ast
