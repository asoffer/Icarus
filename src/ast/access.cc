#include "ast/access.h"

#include "ast/declaration.h"
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
  ASSIGN_OR(return nullptr, auto &operand_type, operand->VerifyType(ctx));

  auto base_type = DereferenceAll(&operand_type);
  if (base_type == type::Type_) {
    // TODO We may not be allowed to evaluate this:
    //    f ::= (T: type) => T.key
    // We need to know that T is const
    auto *evaled_type =
        backend::EvaluateAs<type::Type const *>(operand.get(), ctx);

    // For enums and flags, regardless of whether we can get the value, it's
    // clear that this is supposed to be a member so we should emit an error but
    // carry on assuming that this is an element of that enum type.
    if (auto *e = evaled_type->if_as<type::Enum>()) {
      if (!e->Get(member_name).has_value()) {
        ctx->error_log_.MissingMember(span, member_name, evaled_type);
      }
      return ctx->set_type(this, evaled_type);
    } else if (auto *f = evaled_type->if_as<type::Flags>()) {
      if (!f->Get(member_name).has_value()) {
        ctx->error_log_.MissingMember(span, member_name, evaled_type);
      }
      return ctx->set_type(this, evaled_type);
    } else {
      // TODO what about structs? Can structs have constant members we're
      // allowed to access?
      ctx->error_log_.TypeHasNoMembers(span);
      return nullptr;
    }

  } else if (auto *s = base_type->if_as<type::Struct>()) {
    auto const *member = s->field(member_name);
    if (member == nullptr) {
      ctx->error_log_.MissingMember(span, member_name, s);
      return nullptr;
    }

    if (ctx->mod_ != s->defining_module() &&
        std::none_of(member->hashtags_.begin(), member->hashtags_.end(),
                     [](ast::Hashtag h) {
                       return h.kind_ == ast::Hashtag::Builtin::Export;
                     })) {
      ctx->error_log_.NonExportedMember(span, member_name, s);
    }
    return ctx->set_type(this, member->type);

  } else if (base_type == type::Module) {
    auto *t = backend::EvaluateAs<Module const *>(operand.get(), ctx)
                  ->GetType(member_name);
    if (t == nullptr) {
      ctx->error_log_.NoExportedSymbol(span);
      return nullptr;
    }

    return ctx->set_type(this, t);
  } else {
    ctx->error_log_.MissingMember(span, member_name, base_type);
    return nullptr;
  }
}

void Access::Validate(Context *ctx) { operand->Validate(ctx); }

base::vector<ir::RegisterOr<ir::Addr>> ast::Access::EmitLVal(Context *ctx) {
  auto reg = operand->EmitLVal(ctx)[0];
  auto *t  = ctx->type_of(operand.get());
  if (t->is<type::Pointer>()) { t = t->as<type::Pointer>().pointee; }
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
    auto lit = this_type->as<type::Enum>().EmitLiteral(member_name);
    return {ir::Val(lit)};
  } else if (this_type->is<type::Flags>()) {
    auto lit = this_type->as<type::Flags>().EmitLiteral(member_name);
    return {ir::Val(lit)};
  } else {
    auto lval = EmitLVal(ctx)[0];
    return {ir::Val::Reg(ir::PtrFix(lval.reg_, this_type), this_type)};
  }
}

}  // namespace ast
