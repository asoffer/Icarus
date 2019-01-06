#include "ast/struct_type.h"

#include <sstream>
#include "context.h"
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

VerifyResult StructType::VerifyType(Context *ctx) {
  for (auto &arg : args_) { arg->VerifyType(ctx); }
  return ctx->set_type(this, type::Type_);
}

void StructType::Validate(Context *ctx) {
  for (auto &arg : args_) { arg->Validate(ctx); }
}

base::vector<ir::Val> StructType::EmitIR(Context *ctx) { NOT_YET(); }

base::vector<ir::RegisterOr<ir::Addr>> StructType::EmitLVal(Context *) {
  UNREACHABLE(*this);
}

void StructType::ExtractJumps(JumpExprs *rets) const {
  for (auto &arg : args_) { arg->ExtractJumps(rets); }
}
}  // namespace ast
