#include "ast/unop.h"

#include "core/fn_args.h"
#include "ast/overload_set.h"
#include "ast/terminal.h"
#include "backend/eval.h"
#include "ir/components.h"
#include "ir/func.h"
#include "misc/context.h"

namespace ast {

std::string Unop::to_string(size_t n) const {
  if (op == frontend::Operator::TypeOf) {
    return "(" + operand->to_string(n) + "):?";
  }

  std::stringstream ss;
  switch (op) {
    case frontend::Operator::Which: ss << "which "; break;
    case frontend::Operator::Mul: ss << "*"; break;
    case frontend::Operator::And: ss << "&"; break;
    case frontend::Operator::Sub: ss << "-"; break;
    case frontend::Operator::Not: ss << "!"; break;
    case frontend::Operator::At: ss << "@"; break;
    case frontend::Operator::Eval: ss << "$"; break;
    case frontend::Operator::Needs: ss << "needs "; break;
    case frontend::Operator::Ensure: ss << "ensure "; break;
    case frontend::Operator::Expand: ss << "<< "; break;
    case frontend::Operator::BufPtr: ss << "[*]"; break;
    case frontend::Operator::Copy: ss << "copy "; break;
    case frontend::Operator::Move: ss << "move "; break;
    default: { UNREACHABLE(); }
  }

  ss << operand->to_string(n);
  return ss.str();
}

void Unop::assign_scope(core::Scope *scope) {
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
    case frontend::Operator::Mul: {
      // TODO will this catch buffer pointers too and should it?
      auto *p = t->if_as<type::Pointer>();
      return p && operand->InferType(p->pointee, state);
    }
    case frontend::Operator::Eval: return operand->InferType(t, state);
    case frontend::Operator::BufPtr: {
      auto *p = t->if_as<type::BufferPointer>();
      return p && operand->InferType(p->pointee, state);
    }
    case frontend::Operator::Which:
    case frontend::Operator::And:
    case frontend::Operator::Sub:
    case frontend::Operator::Not:
    case frontend::Operator::At:
    case frontend::Operator::Needs:
    case frontend::Operator::Ensure:
    case frontend::Operator::Expand:
    case frontend::Operator::Copy:
    case frontend::Operator::Move: return false;
    default: UNREACHABLE();
  }
}

void Unop::ExtractJumps(JumpExprs *rets) const { operand->ExtractJumps(rets); }

VerifyResult Unop::VerifyType(Context *ctx) {
  ASSIGN_OR(return VerifyResult::Error(), auto result,
                   operand->VerifyType(ctx));
  auto *operand_type = result.type_;

  switch (op) {
    case frontend::Operator::Copy:
      if (!operand_type->IsCopyable()) {
        NOT_YET("log an error. not copyable");
      }
      // TODO Are copies always consts?
      return VerifyResult(ctx->set_type(this, operand_type), result.const_);
    case frontend::Operator::Move:
      if (!operand_type->IsMovable()) { NOT_YET("log an error. not movable"); }
      // TODO Are copies always consts?
      return VerifyResult(ctx->set_type(this, operand_type), result.const_);
    case frontend::Operator::BufPtr:
      return VerifyResult(ctx->set_type(this, type::Type_), result.const_);
    case frontend::Operator::TypeOf:
      return VerifyResult(ctx->set_type(this, type::Type_), result.const_);
    case frontend::Operator::Eval:
      if (!result.const_) {
        // TODO here you could return a correct type and just have there
        // be an error regarding constness. When you do this probably worth a
        // full pass over all verification code.
        ctx->error_log()->NonConstantEvaluation(operand->span);
        return VerifyResult::Error();
      } else {
        return VerifyResult(ctx->set_type(this, operand_type), result.const_);
      }
    case frontend::Operator::Which:
      if (!operand_type->is<type::Variant>()) {
        ctx->error_log()->WhichNonVariant(operand_type, span);
      }
      return VerifyResult(ctx->set_type(this, type::Type_), result.const_);
    case frontend::Operator::At:
      if (operand_type->is<type::Pointer>()) {
        return VerifyResult(
            ctx->set_type(this, operand_type->as<type::Pointer>().pointee),
            result.const_);
      } else {
        ctx->error_log()->DereferencingNonPointer(operand_type, span);
        return VerifyResult::Error();
      }
    case frontend::Operator::And:
      // TODO  does it make sense to take the address of a constant? I think it
      // has to but it also has to have some special meaning. Things we take the
      // address of in run-time code need to be made available at run-time.
      return VerifyResult(ctx->set_type(this, type::Ptr(operand_type)),
                          result.const_);
    case frontend::Operator::Mul:
      if (operand_type != type::Type_) {
        NOT_YET("log an error, ", operand_type, this);
        return VerifyResult::Error();
      } else {
        return VerifyResult(ctx->set_type(this, type::Type_), result.const_);
      }
    case frontend::Operator::Sub:
      if (type::IsNumeric(operand_type)) {
        return VerifyResult(ctx->set_type(this, operand_type), result.const_);
      } else if (operand_type->is<type::Struct>()) {
        OverloadSet os(scope_, "-", ctx);
        os.add_adl("-", operand_type);

        auto *ret_type = DispatchTable::MakeOrLogError(
            this, core::FnArgs<Expression *>({operand.get()}, {}), os, ctx);
        if (ret_type == nullptr) { return VerifyResult::Error(); }
        return VerifyResult(ctx->set_type(this, ret_type), result.const_);
      }
      NOT_YET();
      return VerifyResult::Error();
    case frontend::Operator::Expand:
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
    case frontend::Operator::Not:
      if (operand_type == type::Bool || operand_type->is<type::Enum>() ||
          operand_type->is<type::Flags>()) {
        return VerifyResult(ctx->set_type(this, operand_type), result.const_);
      }
      if (operand_type->is<type::Struct>()) {
        OverloadSet os(scope_, "!", ctx);
        os.add_adl("!", operand_type);

        auto *ret_type = DispatchTable::MakeOrLogError(
            this, core::FnArgs<Expression *>({operand.get()}, {}), os, ctx);
        if (ret_type == nullptr) { return VerifyResult::Error(); }
        if (ret_type->is<type::Tuple>()) { NOT_YET(); }
        return VerifyResult(ctx->set_type(this, ret_type), result.const_);
      } else {
        NOT_YET("log an error");
        return VerifyResult::Error();
      }
    case frontend::Operator::Needs:
      if (operand_type != type::Bool) {
        ctx->error_log()->PreconditionNeedsBool(operand->span, operand_type);
      }
      if (!result.const_) { NOT_YET(); }
      return VerifyResult::Constant(ctx->set_type(this, type::Void()));
    case frontend::Operator::Ensure:
      if (operand_type != type::Bool) {
        ctx->error_log()->PostconditionNeedsBool(operand->span, operand_type);
      }
      if (!result.const_) { NOT_YET(); }
      return VerifyResult::Constant(ctx->set_type(this, type::Void()));
    default: UNREACHABLE(*this);
  }
}

ir::Results Unop::EmitIr(Context *ctx) {
  auto *operand_type = ctx->type_of(operand.get());
  if (auto const *dispatch_table = ctx->dispatch_table(this)) {
    // TODO struct is not exactly right. we really mean user-defined
    return dispatch_table->EmitCall(
        core::FnArgs<std::pair<Expression *, ir::Results>>(
            {std::pair(operand.get(), operand->EmitIr(ctx))}, {}),
        ctx->type_of(this), ctx);
  }

  switch (op) {
    case frontend::Operator::Copy: {
      auto reg = ir::TmpAlloca(operand_type, ctx);
      type::EmitCopyInit(operand_type, operand->EmitIr(ctx),
                         type::Typed<ir::Register>(reg, operand_type), ctx);
      return ir::Results{reg};
    } break;
    case frontend::Operator::Move: {
      auto reg = ir::TmpAlloca(operand_type, ctx);
      type::EmitMoveInit(operand_type, operand->EmitIr(ctx),
                         type::Typed<ir::Register>(reg, operand_type), ctx);
      return ir::Results{reg};
    } break;
    case frontend::Operator::BufPtr:
      return ir::Results{
          ir::BufPtr(operand->EmitIr(ctx).get<type::Type const *>(0))};
    case frontend::Operator::Not: {
      auto *t = ctx->type_of(operand.get());
      if (t == type::Bool) {
        return ir::Results{ir::Not(operand->EmitIr(ctx).get<bool>(0))};
      } else if (t->is<type::Flags>()) {
        return ir::Results{
            ir::Not(type::Typed<ir::RegisterOr<ir::FlagsVal>, type::Flags>(
                operand->EmitIr(ctx).get<ir::FlagsVal>(0),
                &t->as<type::Flags>()))};
      } else {
        NOT_YET();
      }
    } break;
    case frontend::Operator::Sub: {
      auto operand_ir = operand->EmitIr(ctx);
      return type::ApplyTypes<int8_t, int16_t, int32_t, int64_t, float, double>(
          ctx->type_of(operand.get()), [&](auto type_holder) {
            using T = typename decltype(type_holder)::type;
            return ir::Results{ir::Neg(operand_ir.get<T>(0))};
          });
    } break;
    case frontend::Operator::TypeOf:
      return ir::Results{ctx->type_of(operand.get())};
    case frontend::Operator::Which:
      return ir::Results{ir::Load<type::Type const *>(
          ir::VariantType(operand->EmitIr(ctx).get<ir::Reg>(0)))};
    case frontend::Operator::And: return ir::Results{operand->EmitLVal(ctx)[0]};
    case frontend::Operator::Eval: {
      // Guaranteed to be constant by VerifyType
      // TODO what if there's an error during evaluation?
      return ir::Results::FromVals(backend::Evaluate(operand.get(), ctx));
    }
    case frontend::Operator::Mul:
      return ir::Results{
          ir::Ptr(operand->EmitIr(ctx).get<type::Type const *>(0))};
    case frontend::Operator::At: {
      auto *t = ctx->type_of(this);
      return ir::Results{ir::Load(operand->EmitIr(ctx).get<ir::Reg>(0), t)};
    }
    case frontend::Operator::Needs: {
      // TODO validate requirements are well-formed?
      ir::Func::Current->precondition_exprs_.push_back(operand.get());
      return ir::Results{};
    } break;
    case frontend::Operator::Ensure: {
      // TODO validate requirements are well-formed?
      ir::Func::Current->postcondition_exprs_.push_back(operand.get());
      return ir::Results{};
    } break;
    case frontend::Operator::Expand: {
      ir::Results tuple_val         = operand->EmitIr(ctx);
      ir::Register tuple_reg        = tuple_val.get<ir::Reg>(0);
      type::Tuple const *tuple_type =
          &ctx->type_of(operand.get())->as<type::Tuple>();
      ir::Results results;
      for (size_t i = 0; i < tuple_type->size(); ++i) {
        results.append(ir::PtrFix(ir::Field(tuple_reg, tuple_type, i).get(),
                                  tuple_type->entries_[i]));
      }
      return results;
    }
    default: UNREACHABLE("Operator is ", static_cast<int>(op));
  }
}

std::vector<ir::RegisterOr<ir::Addr>> Unop::EmitLVal(Context *ctx) {
  ASSERT(op == frontend::Operator::At);
  return {operand->EmitIr(ctx).get<ir::Reg>(0)};
}

void Unop::EmitMoveInit(type::Typed<ir::Register> reg, Context *ctx) {
  switch (op) {
    case frontend::Operator::Move: operand->EmitMoveInit(reg, ctx); break;
    case frontend::Operator::Copy: operand->EmitCopyInit(reg, ctx); break;
    default:
      type::EmitMoveInit(ctx->type_of(this), this->EmitIr(ctx), reg, ctx);
      break;
  }
}

void Unop::EmitCopyInit(type::Typed<ir::Register> reg, Context *ctx) {
  switch (op) {
    case frontend::Operator::Move: operand->EmitMoveInit(reg, ctx); break;
    case frontend::Operator::Copy: operand->EmitCopyInit(reg, ctx); break;
    default:
      type::EmitCopyInit(ctx->type_of(this), this->EmitIr(ctx), reg, ctx);
      break;
  }
}

}  // namespace ast
