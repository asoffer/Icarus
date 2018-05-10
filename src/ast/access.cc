#include "ast/access.h"

#include "ast/stages.h"
#include "ast/verify_macros.h"
#include "ir/cmd.h"
#include "module.h"
#include "type/array.h"
#include "type/enum.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/primitive.h"
#include "type/struct.h"

std::vector<IR::Val> Evaluate(AST::Expression *expr, Context *ctx);
IR::Val PtrCallFix(const IR::Val &v);

namespace AST {
namespace {
using base::check::Is;

const type::Type *DereferenceAll(const type::Type *t) {
  while (t->is<type::Pointer>()) { t = t->as<type::Pointer>().pointee; }
  return t;
}
}  // namespace

void Access::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_ = scope;
  operand->assign_scope(scope);
}

void Access::ClearIdDecls() {
  stage_range_ = StageRange{};
  operand->ClearIdDecls();
}

void Access::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  VERIFY_AND_RETURN_ON_ERROR(operand);
  lvalue =
      (operand->type->is<type::Array>() &&
       operand->type->as<type::Array>().fixed_length && member_name == "size")
          ? Assign::Const
          : operand->lvalue;

  auto base_type = DereferenceAll(operand->type);
  if (base_type->is<type::Array>()) {
    if (member_name == "size") {
      type = type::Int;
    } else {
      ctx->error_log_.MissingMember(span, member_name, base_type);
      type = type::Err;
      limit_to(StageRange::Nothing());
    }
  } else if (base_type == type::Type_) {
    auto *evaled_type =
        std::get<const type::Type *>(Evaluate(operand.get(), ctx)[0].value);
    if (evaled_type->is<type::Enum>()) {
      // Regardless of whether we can get the value, it's clear that this is
      // supposed to be a member so we should emit an error but carry on
      // assuming that this is an element of that enum type.
      type = evaled_type;
      if (evaled_type->as<type::Enum>().IntValueOrFail(member_name) ==
          std::numeric_limits<size_t>::max()) {
        ctx->error_log_.MissingMember(span, member_name, evaled_type);
        limit_to(StageRange::NoEmitIR());
      }
    }
  } else if (base_type->is<type::Struct>()) {
    const auto *member = base_type->as<type::Struct>().field(member_name);
    if (member != nullptr) {
      type = member->type;

    } else {
      ctx->error_log_.MissingMember(span, member_name, base_type);
      type = type::Err;
      limit_to(StageRange::Nothing());
    }
  } else if (base_type == type::Module) {
    auto module =
        std::get<const Module *>(Evaluate(operand.get(), ctx)[0].value);
    type = module->GetType(member_name);
    if (type == nullptr) {
      NOT_YET("log an error");
      type = type::Err;
      limit_to(StageRange::Nothing());
    }

  } else if (base_type->is<type::Primitive>() ||
             base_type->is<type::Function>()) {
    ctx->error_log_.MissingMember(span, member_name, base_type);
    type = type::Err;
    limit_to(StageRange::Nothing());
  }
}

void Access::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  operand->Validate(ctx);
}

void Access::contextualize(
    const Node *correspondant,
    const std::unordered_map<const Expression *, IR::Val> &replacements){
  operand->contextualize(correspondant->as<Access>().operand.get(),
                         replacements);
}

IR::Val AST::Access::EmitLVal(Context *ctx) {
  auto val = operand->EmitLVal(ctx);
  while (val.type->is<type::Pointer>() &&
         !val.type->as<type::Pointer>().pointee->is_big()) {
    val = IR::Load(val);
  }

  ASSERT(val.type, Is<type::Pointer>());
  ASSERT(val.type->as<type::Pointer>().pointee, Is<type::Struct>());

  auto *struct_type =
      &val.type->as<type::Pointer>().pointee->as<type::Struct>();
  return IR::Field(val, struct_type->field_indices_ AT(member_name));
}

IR::Val AST::Access::EmitIR(Context *ctx) {
  if (operand->type == type::Module) {
    auto mod =
        std::get<const Module *>(Evaluate(operand.get(), ctx) AT(0).value);
    return mod->GetDecl(member_name)->EmitIR(ctx);
  } else if (type->is<type::Enum>()) {
    return type->as<type::Enum>().EmitLiteral(member_name);
  } else {
    return PtrCallFix(EmitLVal(ctx));
  }
  return IR::Val::None();
}

Access *Access::Clone() const {
  auto *result        = new Access;
  result->span        = span;
  result->operand     = base::wrap_unique(operand->Clone());
  result->member_name = member_name;
  return result;
}

}  // namespace AST
