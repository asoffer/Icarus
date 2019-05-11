#include "ast/unop.h"

#include "ast/overload_set.h"
#include "ast/terminal.h"
#include "backend/eval.h"
#include "core/fn_args.h"
#include "ir/components.h"
#include "ir/compiled_fn.h"
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

void Unop::DependentDecls(DeclDepGraph *g, Declaration *d) const {
  operand->DependentDecls(g, d);
}

bool Unop::InferType(type::Type const *t, InferenceState *state) const {
  // TODO consider the possibility for overloadable operators to be generic
  // struct and therefore not always returning false.
  switch (op) {
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

ir::Results Unop::EmitIr(Context *ctx) {
  auto *operand_type = ctx->type_of(operand.get());
  if (auto const *dispatch_table = ctx->dispatch_table(this)) {
    // TODO struct is not exactly right. we really mean user-defined
    return dispatch_table->EmitCall(
        core::FnArgs<std::pair<Expression const *, ir::Results>>(
            {std::pair(operand.get(), operand->EmitIr(ctx))}, {}),
        ctx);
  }

  switch (op) {
    case frontend::Operator::Copy: {
      auto reg = ir::TmpAlloca(operand_type, ctx);
      type::EmitCopyInit(operand_type, operand->EmitIr(ctx),
                         type::Typed<ir::Reg>(reg, operand_type), ctx);
      return ir::Results{reg};
    } break;
    case frontend::Operator::Move: {
      auto reg = ir::TmpAlloca(operand_type, ctx);
      type::EmitMoveInit(operand_type, operand->EmitIr(ctx),
                         type::Typed<ir::Reg>(reg, operand_type), ctx);
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
      return backend::Evaluate(operand.get(), ctx);
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
      ir::CompiledFn::Current->precondition_exprs_.push_back(operand.get());
      return ir::Results{};
    } break;
    case frontend::Operator::Ensure: {
      // TODO validate requirements are well-formed?
      ir::CompiledFn::Current->postcondition_exprs_.push_back(operand.get());
      return ir::Results{};
    } break;
    case frontend::Operator::Expand: {
      ir::Results tuple_val  = operand->EmitIr(ctx);
      ir::Reg tuple_reg = tuple_val.get<ir::Reg>(0);
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

void Unop::EmitMoveInit(type::Typed<ir::Reg> reg, Context *ctx) {
  switch (op) {
    case frontend::Operator::Move: operand->EmitMoveInit(reg, ctx); break;
    case frontend::Operator::Copy: operand->EmitCopyInit(reg, ctx); break;
    default:
      type::EmitMoveInit(ctx->type_of(this), this->EmitIr(ctx), reg, ctx);
      break;
  }
}

void Unop::EmitCopyInit(type::Typed<ir::Reg> reg, Context *ctx) {
  switch (op) {
    case frontend::Operator::Move: operand->EmitMoveInit(reg, ctx); break;
    case frontend::Operator::Copy: operand->EmitCopyInit(reg, ctx); break;
    default:
      type::EmitCopyInit(ctx->type_of(this), this->EmitIr(ctx), reg, ctx);
      break;
  }
}

}  // namespace ast
