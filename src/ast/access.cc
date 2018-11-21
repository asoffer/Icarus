#include "ast/access.h"

#include "ast/verify_macros.h"
#include "backend/eval.h"
#include "ir/cmd.h"
#include "ir/components.h"
#include "module.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/struct.h"

namespace ast {
namespace {
using base::check::Is;

const type::Type *DereferenceAll(const type::Type *t) {
  while (t->is<type::Pointer>()) { t = t->as<type::Pointer>().pointee; }
  return t;
}
}  // namespace

void Access::assign_scope(Scope *scope) {
  scope_ = scope;
  operand->assign_scope(scope);
}

type::Type const *Access::VerifyType(Context *ctx) {
  VERIFY_OR_RETURN(operand_type, operand);

  auto base_type = DereferenceAll(operand_type);
  if (base_type == type::Type_) {
    auto *evaled_type =
        backend::EvaluateAs<const type::Type *>(operand.get(), ctx);
    if (evaled_type->is<type::Enum>()) {
      // Regardless of whether we can get the value, it's clear that this is
      // supposed to be a member so we should emit an error but carry on
      // assuming that this is an element of that enum type.
      ctx->set_type(this, evaled_type);
      if (evaled_type->as<type::Enum>().IntValueOrFail(member_name) ==
          std::numeric_limits<size_t>::max()) {
        ctx->error_log_.MissingMember(span, member_name, evaled_type);
      }
    } else if (evaled_type->is<type::Flags>()) {
      // Regardless of whether we can get the value, it's clear that this is
      // supposed to be a member so we should emit an error but carry on
      // assuming that this is an element of that enum type.
      ctx->set_type(this, evaled_type);
      if (evaled_type->as<type::Flags>().IntValueOrFail(member_name) ==
          std::numeric_limits<size_t>::max()) {
        ctx->error_log_.MissingMember(span, member_name, evaled_type);
      }
    }
    return evaled_type;
  } else if (base_type->is<type::Struct>()) {
    const auto *member = base_type->as<type::Struct>().field(member_name);
    if (member != nullptr) {
      ctx->set_type(this, member->type);
      return member->type;

    } else {
      ctx->error_log_.MissingMember(span, member_name, base_type);
      return nullptr;
    }
  } else if (base_type == type::Module) {
    auto *t = backend::EvaluateAs<Module const *>(operand.get(), ctx)
                  ->GetType(member_name);
    ctx->set_type(this, t);
    if (t == nullptr) {
      NOT_YET("log an error");
    }
    return t;
  } else {
    ctx->error_log_.MissingMember(span, member_name, base_type);
    return nullptr;
  }
}

void Access::Validate(Context *ctx) { operand->Validate(ctx); }

base::vector<ir::Register> ast::Access::EmitLVal(Context *ctx) {
  auto reg   = operand->EmitLVal(ctx)[0];
  auto *t    = ctx->type_of(operand.get());
  while (t->is<type::Pointer>()) {
    t   = t->as<type::Pointer>().pointee;
    reg = ir::Load<ir::Addr>(reg, t);
  }

  ASSERT(t, Is<type::Struct>());
  auto *struct_type = &t->as<type::Struct>();
  return {ir::Field(reg, struct_type, struct_type->index(member_name))};
}

base::vector<ir::Val> ast::Access::EmitIR(Context *ctx) {
  if (ctx->type_of(operand.get()) == type::Module) {
    return backend::EvaluateAs<Module const *>(operand.get(), ctx)
        ->GetDecl(member_name)
        ->EmitIR(ctx);
  }

  auto *this_type = ctx->type_of(this);
  if (this_type->is<type::Enum>()) {
    return {this_type->as<type::Enum>().EmitLiteral(member_name)};
  } else if (this_type->is<type::Flags>()) {
    return {this_type->as<type::Flags>().EmitLiteral(member_name)};
  } else {
    return {ir::Val::Reg(ir::PtrFix(EmitLVal(ctx)[0], this_type), this_type)};
  }
}

}  // namespace ast
