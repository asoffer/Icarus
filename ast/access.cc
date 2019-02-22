#include "ast/access.h"

#include "ast/declaration.h"
#include "backend/eval.h"
#include "ir/cmd.h"
#include "ir/components.h"
#include "misc/module.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/flags.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/struct.h"

namespace ast {
namespace {
using ::matcher::InheritsFrom;

type::Type const *DereferenceAll(type::Type const *t) {
  while (auto *p = t->if_as<type::Pointer>()) { t = p->pointee; }
  return t;
}

}  // namespace

void Access::assign_scope(Scope *scope) {
  scope_ = scope;
  operand->assign_scope(scope);
}

void Access::DependentDecls(base::Graph<Declaration *> *g,
                            Declaration *d) const {
  operand->DependentDecls(g, d);
}

bool Access::InferType(type::Type const *t, InferenceState *state) const {
  return false;
}

VerifyResult Access::VerifyType(Context *ctx) {
  ASSIGN_OR(return VerifyResult::Error(), auto operand_result,
                   operand->VerifyType(ctx));

  auto base_type = DereferenceAll(operand_result.type_);
  if (base_type == type::Type_) {
    if (!operand_result.const_) {
      ctx->error_log()->NonConstantTypeMemberAccess(span);
      return VerifyResult::Error();
    }
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
        ctx->error_log()->MissingMember(span, member_name, evaled_type);
      }
      return VerifyResult::Constant(ctx->set_type(this, evaled_type));
    } else if (auto *f = evaled_type->if_as<type::Flags>()) {
      if (!f->Get(member_name).has_value()) {
        ctx->error_log()->MissingMember(span, member_name, evaled_type);
      }
      return VerifyResult::Constant(ctx->set_type(this, evaled_type));
    } else {
      // TODO what about structs? Can structs have constant members we're
      // allowed to access?
      ctx->error_log()->TypeHasNoMembers(span);
      return VerifyResult::Error();
    }

  } else if (auto *s = base_type->if_as<type::Struct>()) {
    auto const *member = s->field(member_name);
    if (member == nullptr) {
      ctx->error_log()->MissingMember(span, member_name, s);
      return VerifyResult::Error();
    }

    if (ctx->mod_ != s->defining_module() &&
        std::none_of(
            member->hashtags_.begin(), member->hashtags_.end(),
            [](Hashtag h) { return h.kind_ == Hashtag::Builtin::Export; })) {
      ctx->error_log()->NonExportedMember(span, member_name, s);
    }

    return VerifyResult(ctx->set_type(this, member->type),
                        operand_result.const_);

  } else if (base_type == type::Module) {
    if (!operand_result.const_) {
      ctx->error_log()->NonConstantModuleMemberAccess(span);
      return VerifyResult::Error();
    }

    auto *t = backend::EvaluateAs<Module const *>(operand.get(), ctx)
                  ->GetType(member_name);
    if (t == nullptr) {
      ctx->error_log()->NoExportedSymbol(span);
      return VerifyResult::Error();
    }

    // TODO is this right?
    return VerifyResult::Constant(ctx->set_type(this, t));
  } else {
    ctx->error_log()->MissingMember(span, member_name, base_type);
    return VerifyResult::Error();
  }
}

std::vector<ir::RegisterOr<ir::Addr>> Access::EmitLVal(Context *ctx) {
  auto reg = operand->EmitLVal(ctx)[0];
  auto *t  = ctx->type_of(operand.get());

  while (t->is<type::Pointer>()) {
    t   = t->as<type::Pointer>().pointee;
    reg = ir::Load<ir::Addr>(reg, t);
  }

  ASSERT(t, InheritsFrom<type::Struct>());
  auto *struct_type = &t->as<type::Struct>();
  return {ir::Field(reg, struct_type, struct_type->index(member_name)).get()};
}

ir::Results Access::EmitIr(Context *ctx) {
  if (ctx->type_of(operand.get()) == type::Module) {
    // TODO we already did this evaluation in type verification. Can't we just
    // save and reuse it?
    return backend::EvaluateAs<Module const *>(operand.get(), ctx)
        ->GetDecl(member_name)
        ->EmitIr(ctx);
  }

  auto *this_type = ctx->type_of(this);
  if (this_type->is<type::Enum>()) {
    auto lit = this_type->as<type::Enum>().EmitLiteral(member_name);
    return ir::Results{lit};
  } else if (this_type->is<type::Flags>()) {
    auto lit = this_type->as<type::Flags>().EmitLiteral(member_name);
    return ir::Results{lit};
  } else {
    auto reg = operand->EmitLVal(ctx)[0];
    auto *t  = ctx->type_of(operand.get());

    if (t->is<type::Pointer>()) { t = t->as<type::Pointer>().pointee; }
    while (t->is<type::Pointer>()) {
      t   = t->as<type::Pointer>().pointee;
      reg = ir::Load<ir::Addr>(reg, t);
    }

    ASSERT(t, InheritsFrom<type::Struct>());
    auto *struct_type = &t->as<type::Struct>();
    auto field = ir::Field(reg, struct_type, struct_type->index(member_name));
    return ir::Results{ir::PtrFix(field.get(), this_type)};
  }
}

// Just sticking this in some cc file for now. I don't really care where. Just
// so ir::Val is complete. Going to delete it soon.
ir::Results Node::EmitIr(Context *ctx) {
  ir::Results results;
  for (auto const &val : EmitIR(ctx)) {
    std::visit([&results](auto x) { results.append(x); }, val.value);
  }
  return results;
}

std::vector<ir::Val> Expression::EmitIR(Context *ctx) {
  auto results = EmitIr(ctx);
  auto *t      = ctx->type_of(this);
  if (t == nullptr) {
    return {};
  } else if (auto *e = t->if_as<type::Enum>()) {
    return {ir::ValFrom(results.get<ir::EnumVal>(0).val_, e)};
  } else if (auto *f = t->if_as<type::Flags>()) {
    return {ir::ValFrom(results.get<ir::FlagsVal>(0).val_, f)};
  } else if (auto *fn = t->if_as<type::Function>()) {
    auto f = results.get<ir::AnyFunc>(0);
    if (f.is_reg_) { return {ir::Val::Reg(f.reg_, fn)}; }
    return {ir::Val::Func(fn, f.val_)};
  } else if (auto *v = t->if_as<type::Variant>()) {
    return {ir::ValFrom(results.get<ir::Addr>(0), type::Ptr(v))};
  }
  return {
      type::ApplyTypes<bool, int8_t, int16_t, int32_t, int64_t, uint8_t,
                       uint16_t, uint32_t, uint64_t, float, double, ir::Addr,
                       ::Module *, ast::ScopeLiteral *, ast::FunctionLiteral *,
                       std::string_view, type::Type const *, ir::BlockSequence>(
          t, [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            return ir::ValFrom(results.get<T>(0));
          })};
}

std::vector<ir::Val> Node::EmitIR(Context *ctx) {
  EmitIr(ctx);
  return {};
}

}  // namespace ast
