#include "ast/array_literal.h"

#include "context.h"
#include "error/log.h"
#include "ir/cmd.h"
#include "type/array.h"
#include "type/cast.h"
#include "type/pointer.h"

namespace ast {
std::string ArrayLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << "[";
  auto iter = cl_.exprs_.begin();
  ss << (*iter)->to_string(n);
  ++iter;
  while (iter != cl_.exprs_.end()) {
    ss << ", " << (*iter)->to_string(n);
    ++iter;
  }
  ss << "]";
  return ss.str();
}

type::Type const *ArrayLiteral::VerifyType(Context *ctx) {
  if (cl_.exprs_.empty()) {
    ctx->set_type(this, type::EmptyArray);
    return type::EmptyArray;
  }

  ASSIGN_OR(return nullptr, auto expr_types, cl_.VerifyWithoutSetting(ctx));
  if (std::all_of(
          expr_types.begin(), expr_types.end(),
          [&](type::Type const *t) { return t == expr_types.front(); })) {
    return ctx->set_type(this,
                         type::Arr(expr_types.front(), cl_.exprs_.size()));
  } else {
    ctx->error_log_.InconsistentArrayType(span);
    return nullptr;
  }
}

base::vector<ir::Val> ast::ArrayLiteral::EmitIR(Context *ctx) {
  // TODO If this is a constant we can just store it somewhere.
  auto *this_type = ctx->type_of(this);
  auto alloc      = ir::TmpAlloca(this_type, ctx);
  auto array_val  = ir::Val::Reg(alloc, type::Ptr(this_type));
  auto *data_type = this_type->as<type::Array>().data_type;
  for (size_t i = 0; i < cl_.exprs_.size(); ++i) {
    type::EmitMoveInit(
        data_type, data_type, cl_.exprs_[i]->EmitIR(ctx)[0],
        ir::Index(type::Ptr(this_type), alloc, static_cast<i32>(i)), ctx);
  }
  return {array_val};
}

base::vector<ir::RegisterOr<ir::Addr>> ast::ArrayLiteral::EmitLVal(
    Context *ctx) {
  UNREACHABLE(*this);
}
}  // namespace ast
