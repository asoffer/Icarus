#include "ast/array_literal.h"

#include "context.h"
#include "error/log.h"
#include "ir/cmd.h"
#include "type/array.h"
#include "type/pointer.h"

namespace ast {
void ArrayLiteral::assign_scope(Scope *scope) {
  scope_ = scope;
  for (auto &el : elems_) { el->assign_scope(scope); }
}

std::string ArrayLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << "[";
  auto iter = elems_.begin();
  ss << (*iter)->to_string(n);
  ++iter;
  while (iter != elems_.end()) {
    ss << ", " << (*iter)->to_string(n);
    ++iter;
  }
  ss << "]";
  return ss.str();
}

type::Type const *ArrayLiteral::VerifyType(Context *ctx) {
  if (elems_.empty()) {
    ctx->set_type(this, type::EmptyArray);
    return type::EmptyArray;
  }

  std::vector<type::Type const *> elem_types;
  elem_types.reserve(elems_.size());
  for (auto &elem : elems_) { elem_types.push_back(elem->VerifyType(ctx)); }
  if (std::any_of(elem_types.begin(), elem_types.end(),
                  [](type::Type const *t) { return t == nullptr; })) {
    return nullptr;
  }

  const type::Type *joined = nullptr;
  for (auto *elem_type : elem_types) { joined = type::Join(joined, elem_type); }

  if (joined == nullptr) {
    // type::Types couldn't be joined. Emit an error
    ctx->error_log_.InconsistentArrayType(span);
    return nullptr;
  } else {
    return ctx->set_type(this, type::Arr(joined, elems_.size()));
  }
}

void ArrayLiteral::Validate(Context *ctx) {
  for (auto &elem : elems_) { elem->Validate(ctx); }
}

void ArrayLiteral::ExtractJumps(JumpExprs *rets) const {
  for (auto &el : elems_) { el->ExtractJumps(rets); }
}

base::vector<ir::Val> ast::ArrayLiteral::EmitIR(Context *ctx) {
  // TODO If this is a constant we can just store it somewhere.
  auto *this_type = ctx->type_of(this);
  auto alloc      = ir::Alloca(this_type);
  auto array_val  = ir::Val::Reg(alloc, type::Ptr(this_type));
  auto *data_type = this_type->as<type::Array>().data_type;
  for (size_t i = 0; i < elems_.size(); ++i) {
    type::EmitMoveInit(
        data_type, data_type, elems_[i]->EmitIR(ctx)[0],
        ir::Index(type::Ptr(this_type), alloc, static_cast<i32>(i)), ctx);
  }
  return {array_val};
}

base::vector<ir::Register> ast::ArrayLiteral::EmitLVal(Context *ctx) {
  UNREACHABLE(*this);
}
}  // namespace ast
