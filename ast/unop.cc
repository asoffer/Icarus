#include "ast/unop.h"

#include "ast/fn_args.h"
#include "ast/overload_set.h"
#include "ast/terminal.h"
#include "backend/eval.h"
#include "base/check.h"
#include "ir/components.h"
#include "ir/func.h"
#include "misc/context.h"

namespace ast {
using base::check::Is;
using base::check::Not;

std::string Unop::to_string(size_t n) const {
  if (op == Language::Operator::TypeOf) {
    return "(" + operand->to_string(n) + "):?";
  }

  std::stringstream ss;
  switch (op) {
    case Language::Operator::Which: ss << "which "; break;
    case Language::Operator::Mul: ss << "*"; break;
    case Language::Operator::And: ss << "&"; break;
    case Language::Operator::Sub: ss << "-"; break;
    case Language::Operator::Not: ss << "!"; break;
    case Language::Operator::At: ss << "@"; break;
    case Language::Operator::Eval: ss << "$"; break;
    case Language::Operator::Needs: ss << "needs "; break;
    case Language::Operator::Ensure: ss << "ensure "; break;
    case Language::Operator::Expand: ss << "<< "; break;
    case Language::Operator::BufPtr: ss << "[*]"; break;
    case Language::Operator::Copy: ss << "copy "; break;
    case Language::Operator::Move: ss << "move "; break;
    default: { UNREACHABLE(); }
  }

  ss << operand->to_string(n);
  return ss.str();
}

void Unop::assign_scope(Scope *scope) {
  scope_ = scope;
  operand->assign_scope(scope);
}

void Unop::DependentDecls(base::Graph<Declaration *> *g,
                            Declaration *d) const {
  operand->DependentDecls(g, d);
}

bool Unop::InferType(type::Type const *t, InferenceState *state) const {
  // TODO consider the possibility for overloadable operators to be generic
  // struct and therefore not always returning false.
  switch  (op) {
    case Language::Operator::Mul: {
      // TODO will this catch buffer pointers too and should it?
      auto *p = t->if_as<type::Pointer>();
      return p && operand->InferType(p->pointee, state);
    }
    case Language::Operator::Eval: return operand->InferType(t, state);
    case Language::Operator::BufPtr: {
      auto *p = t->if_as<type::BufferPointer>();
      return p && operand->InferType(p->pointee, state);
    }
    case Language::Operator::Which:
    case Language::Operator::And:
    case Language::Operator::Sub:
    case Language::Operator::Not:
    case Language::Operator::At:
    case Language::Operator::Needs:
    case Language::Operator::Ensure:
    case Language::Operator::Expand:
    case Language::Operator::Copy:
    case Language::Operator::Move: return false;
    default: UNREACHABLE();
  }
}

void Unop::ExtractJumps(JumpExprs *rets) const { operand->ExtractJumps(rets); }

VerifyResult Unop::VerifyType(Context *ctx) {
  ASSIGN_OR(return VerifyResult::Error(), auto result,
                   operand->VerifyType(ctx));
  auto *operand_type = result.type_;

  switch (op) {
    case Language::Operator::Copy:
      if (!operand_type->IsCopyable()) {
        NOT_YET("log an error. not copyable");
      }
      // TODO Are copies always consts?
      return VerifyResult(ctx->set_type(this, operand_type), result.const_);
    case Language::Operator::Move:
      if (!operand_type->IsMovable()) { NOT_YET("log an error. not movable"); }
      // TODO Are copies always consts?
      return VerifyResult(ctx->set_type(this, operand_type), result.const_);
    case Language::Operator::BufPtr:
      return VerifyResult(ctx->set_type(this, type::Type_), result.const_);
    case Language::Operator::TypeOf:
      return VerifyResult(ctx->set_type(this, type::Type_), result.const_);
    case Language::Operator::Eval:
      if (!result.const_) {
        // TODO here you could return a correct type and just have there
        // be an error regarding constness. When you do this probably worth a
        // full pass over all verification code.
        ctx->error_log_.NonConstantEvaluation(operand->span);
        return VerifyResult::Error();
      } else {
        return VerifyResult(ctx->set_type(this, operand_type), result.const_);
      }
    case Language::Operator::Which:
      if (!operand_type->is<type::Variant>()) {
        ctx->error_log_.WhichNonVariant(operand_type, span);
      }
      return VerifyResult(ctx->set_type(this, type::Type_), result.const_);
    case Language::Operator::At:
      if (operand_type->is<type::Pointer>()) {
        return VerifyResult(
            ctx->set_type(this, operand_type->as<type::Pointer>().pointee),
            result.const_);
      } else {
        ctx->error_log_.DereferencingNonPointer(operand_type, span);
        return VerifyResult::Error();
      }
    case Language::Operator::And:
      // TODO  does it make sense to take the address of a constant? I think it
      // has to but it also has to have some special meaning. Things we take the
      // address of in run-time code need to be made available at run-time.
      return VerifyResult(ctx->set_type(this, type::Ptr(operand_type)),
                          result.const_);
    case Language::Operator::Mul:
      if (operand_type != type::Type_) {
        NOT_YET("log an error, ", operand_type, this);
        return VerifyResult::Error();
      } else {
        return VerifyResult(ctx->set_type(this, type::Type_), result.const_);
      }
    case Language::Operator::Sub:
      if (type::IsNumeric(operand_type)) {
        return VerifyResult(ctx->set_type(this, operand_type), result.const_);
      } else if (operand_type->is<type::Struct>()) {
        FnArgs<Expression *> args;
        args.pos_ = std::vector<Expression *>{operand.get()};
        OverloadSet os(scope_, "-", ctx);
        os.add_adl("-", operand_type);

        auto *ret_type = DispatchTable::MakeOrLogError(this, args, os, ctx);
        if (ret_type == nullptr) { return VerifyResult::Error(); }
        return VerifyResult(ctx->set_type(this, ret_type), result.const_);
      }
      NOT_YET();
      return VerifyResult::Error();
    case Language::Operator::Expand:
      // NOTE: It doesn't really make sense to ask for the type of an expanded
      // argument, but since we consider the type of the result of a function
      // call returning multiple arguments to be a tuple, we do the same here.
      //
      if (operand_type->is<type::Tuple>()) {
        // TODO there should be a way to avoid copying over any of entire type
        return VerifyResult(ctx->set_type(this, operand_type), result.const_);
      } else {
        NOT_YET();  // Log an error. can't expand a non-tuple.
      }
    case Language::Operator::Not:
      if (operand_type == type::Bool || operand_type->is<type::Enum>() ||
          operand_type->is<type::Flags>()) {
        return VerifyResult(ctx->set_type(this, operand_type), result.const_);
      }
      if (operand_type->is<type::Struct>()) {
        FnArgs<Expression *> args;
        args.pos_ = std::vector<Expression *>{operand.get()};
        OverloadSet os(scope_, "!", ctx);
        os.add_adl("!", operand_type);

        auto *ret_type = DispatchTable::MakeOrLogError(this, args, os, ctx);
        if (ret_type == nullptr) { return VerifyResult::Error(); }
        if (ret_type->is<type::Tuple>()) { NOT_YET(); }
        return VerifyResult(ctx->set_type(this, ret_type), result.const_);
      } else {
        NOT_YET("log an error");
        return VerifyResult::Error();
      }
    case Language::Operator::Needs:
      if (operand_type != type::Bool) {
        ctx->error_log_.PreconditionNeedsBool(operand->span, operand_type);
      }
      if (!result.const_) { NOT_YET(); }
      return VerifyResult::Constant(ctx->set_type(this, type::Void()));
    case Language::Operator::Ensure:
      if (operand_type != type::Bool) {
        ctx->error_log_.PostconditionNeedsBool(operand->span, operand_type);
      }
      if (!result.const_) { NOT_YET(); }
      return VerifyResult::Constant(ctx->set_type(this, type::Void()));
    default: UNREACHABLE(*this);
  }
}

std::vector<ir::Val> Unop::EmitIR(Context *ctx) {
  auto *operand_type = ctx->type_of(operand.get());
  if (auto const *dispatch_table = ctx->dispatch_table(this)) {
    // TODO struct is not exactly right. we really mean user-defined
    FnArgs<std::pair<Expression *, std::vector<ir::Val>>> args;
    args.pos_ = {std::pair(operand.get(), operand->EmitIR(ctx))};
    return dispatch_table->EmitCall(args, ctx->type_of(this), ctx);
  }

  switch (op) {
    case Language::Operator::Copy: {
      auto reg = ir::TmpAlloca(operand_type, ctx);
      type::EmitCopyInit(operand_type, operand->EmitIR(ctx)[0],
                         type::Typed<ir::Register>(reg, operand_type), ctx);
      return {ir::Val::Reg(reg, operand_type)};
    } break;
    case Language::Operator::Move: {
      auto reg = ir::TmpAlloca(operand_type, ctx);
      type::EmitMoveInit(operand_type, operand->EmitIR(ctx)[0],
                         type::Typed<ir::Register>(reg, operand_type), ctx);
      return {ir::Val::Reg(reg, operand_type)};
    } break;
    case Language::Operator::BufPtr:
      return {ir::ValFrom(
          ir::BufPtr(operand->EmitIR(ctx)[0].reg_or<type::Type const *>()))};
    case Language::Operator::Not: {
      auto *t = ctx->type_of(operand.get());
      if (t == type::Bool) {
        return {ir::ValFrom(ir::Not(operand->EmitIR(ctx)[0].reg_or<bool>()))};
      } else if (t->is<type::Flags>()) {
        auto *flags_type = &t->as<type::Flags>();
        return {ir::ValFrom(
            ir::Not(type::Typed<ir::RegisterOr<ir::FlagsVal>, type::Flags>(
                operand->EmitIR(ctx)[0].reg_or<ir::FlagsVal>(), flags_type)),
            flags_type)};
      } else {
        NOT_YET();
      }
    } break;
    case Language::Operator::Sub: {
      auto operand_ir = operand->EmitIR(ctx)[0];
      return {
          type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, float, double>(
              ctx->type_of(operand.get()), [&](auto type_holder) {
                using T = typename decltype(type_holder)::type;
                return ir::ValFrom(ir::Neg(operand_ir.reg_or<T>()));
              })};
    } break;
    case Language::Operator::TypeOf:
      return {ir::Val(ctx->type_of(operand.get()))};
    case Language::Operator::Which:
      return {ir::Val::Reg(
          ir::Load<type::Type const *>(ir::VariantType(
              std::get<ir::Register>(operand->EmitIR(ctx)[0].value))),
          type::Type_)};
    case Language::Operator::And:
      return {ir::ValFrom(operand->EmitLVal(ctx)[0],
                          type::Ptr(ctx->type_of(this)))};
    case Language::Operator::Eval: {
      // Guaranteed to be constant by VerifyType
      // TODO what if there's an error during evaluation?
      return backend::Evaluate(operand.get(), ctx);
    }
    case Language::Operator::Mul:
      return {ir::ValFrom(
          ir::Ptr(operand->EmitIR(ctx)[0].reg_or<type::Type const *>()))};
    case Language::Operator::At: {
      auto *t = ctx->type_of(this);
      return {ir::Val::Reg(
          ir::Load(std::get<ir::Register>(operand->EmitIR(ctx)[0].value), t),
          t)};
    }
    case Language::Operator::Needs: {
      // TODO validate requirements are well-formed?
      ir::Func::Current->precondition_exprs_.push_back(operand.get());
      return {};
    } break;
    case Language::Operator::Ensure: {
      // TODO validate requirements are well-formed?
      ir::Func::Current->postcondition_exprs_.push_back(operand.get());
      return {};
    } break;
    case Language::Operator::Expand: {
      ir::Val tuple_val             = operand->EmitIR(ctx)[0];
      ir::Register tuple_reg        = std::get<ir::Register>(tuple_val.value);
      type::Tuple const *tuple_type = &tuple_val.type->as<type::Tuple>();
      std::vector<ir::Val> results;
      results.reserve(tuple_type->size());
      for (size_t i = 0; i < tuple_type->size(); ++i) {
        results.push_back(
            ir::Val::Reg(ir::PtrFix(ir::Field(tuple_reg, tuple_type, i).get(),
                                    tuple_type->entries_[i]),
                         tuple_type->entries_[i]));
      }
      return results;
    }
    default: UNREACHABLE("Operator is ", static_cast<int>(op));
  }
}

std::vector<ir::RegisterOr<ir::Addr>> Unop::EmitLVal(Context *ctx) {
  ASSERT(op == Language::Operator::At);
  return {std::get<ir::Register>(operand->EmitIR(ctx)[0].value)};
}

void Unop::EmitMoveInit(type::Typed<ir::Register> reg, Context *ctx) {
  switch (op) {
    case Language::Operator::Move: operand->EmitMoveInit(reg, ctx); break;
    case Language::Operator::Copy: operand->EmitCopyInit(reg, ctx); break;
    default:
      type::EmitMoveInit(ctx->type_of(this), this->EmitIR(ctx)[0], reg, ctx);
      break;
  }
}

void Unop::EmitCopyInit(type::Typed<ir::Register> reg, Context *ctx) {
  switch (op) {
    case Language::Operator::Move: operand->EmitMoveInit(reg, ctx); break;
    case Language::Operator::Copy: operand->EmitCopyInit(reg, ctx); break;
    default:
      type::EmitCopyInit(ctx->type_of(this), this->EmitIR(ctx)[0], reg, ctx);
      break;
  }
}

}  // namespace ast
