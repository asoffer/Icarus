#include "ast/function_literal.h"

#include <sstream>
#include "ast/bound_constants.h"
#include "ast/declaration.h"
#include "ast/terminal.h"
#include "ast/verify_macros.h"
#include "context.h"
#include "error/log.h"
#include "ir/func.h"
#include "module.h"
#include "type/function.h"
#include "type/tuple.h"
#include "type/type.h"

std::vector<IR::Val> Evaluate(AST::Expression *expr, Context *ctx);

namespace AST {
std::string FunctionLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << "(";
  if (!inputs.empty()) {
    auto iter = inputs.begin();
    ss << (*iter)->to_string(n);
    ++iter;
    while (iter != inputs.end()) {
      ss << ", " << (*iter)->to_string(n);
      ++iter;
    }
  }
  ss << ") -> ";
  if (!return_type_inferred_) {
    ss << "(";
    if (!outputs.empty()) {
      auto iter = outputs.begin();
      ss << (*iter)->to_string(n);
      ++iter;
      while (iter != outputs.end()) {
        ss << ", " << (*iter)->to_string(n);
        ++iter;
      }
    }
    ss << ")";
  }
  ss << " {\n"
     << statements->to_string(n + 1) << std::string(2 * n, ' ') << "}";
  return ss.str();
}

std::string GenericFunctionLiteral::to_string(size_t n) const {
  return FunctionLiteral::to_string(n);
}

void FunctionLiteral::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_ = scope;
  if (!fn_scope) {
    fn_scope         = scope->add_child<FnScope>();
    fn_scope->fn_lit = this;
  }
  for (auto &in : inputs) { in->assign_scope(fn_scope.get()); }
  for (auto &out : outputs) { out->assign_scope(fn_scope.get()); }
  statements->assign_scope(fn_scope.get());
}

void GenericFunctionLiteral::assign_scope(Scope *scope) {
  FunctionLiteral::assign_scope(scope);
}

void FunctionLiteral::ClearIdDecls() {
  fn_scope     = nullptr;
  stage_range_ = StageRange{};
  for (auto &in : inputs) { in->ClearIdDecls(); }
  for (auto &out : outputs) { out->ClearIdDecls(); }
  statements->ClearIdDecls();
}

void GenericFunctionLiteral::ClearIdDecls() {
  stage_range_ = StageRange{};
  FunctionLiteral::ClearIdDecls();
}

void FunctionLiteral::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  lvalue = Assign::Const;
  for (auto &input : inputs) {
    input->VerifyType(ctx);
    HANDLE_CYCLIC_DEPENDENCIES;
  }

  for (auto &output : outputs) {
    output->VerifyType(ctx);
    HANDLE_CYCLIC_DEPENDENCIES;
  }

  if (ctx->num_errors() > 0) {
    type = type::Err;
    limit_to(StageRange::Nothing());
    return;
  }

  if (!return_type_inferred_) {
    std::vector<const type::Type *> input_type_vec;
    input_type_vec.reserve(inputs.size());
    for (const auto &input : inputs) { input_type_vec.push_back(input->type); }

    // TODO should named return types be required?
    std::vector<IR::Val> out_vals;
    out_vals.reserve(outputs.size());
    for (const auto &out : outputs) {
      auto result = Evaluate(
          [&]() {
            if (!out->is<Declaration>()) { return out.get(); }

            auto *out_decl = &out->as<Declaration>();
            if (out_decl->IsInferred()) { NOT_YET(); }
            return out_decl->type_expr.get();
          }(),
          ctx);
      ASSERT(result.size() == 1u);
      out_vals.push_back(std::move(result)[0]);
    }

    std::vector<const type::Type *> ret_types;
    ret_types.reserve(out_vals.size());
    for (size_t i = 0; i < out_vals.size(); ++i) {
      const auto &out = out_vals[i];
      if (out == IR::Val::None() /* TODO Error() */) {
        ctx->error_log_.IndeterminantType(outputs[i]->span);
        type = type::Err;
        limit_to(StageRange::Nothing());
      } else if (out.type != type::Type_) {
        ctx->error_log_.NotAType(outputs[i].get());
        type = type::Err;
        limit_to(StageRange::Nothing());
        continue;
      } else if (auto *ret_type = std::get<const type::Type *>(out.value);
                 ret_type == type::Err) {
        type = type::Err;
        limit_to(StageRange::Nothing());
        continue;
      } else {
        ret_types.push_back(ret_type);
      }
    }

    type = type::Func(std::move(input_type_vec), std::move(ret_types));
  } else {
    Validate(ctx);
  }
}

void GenericFunctionLiteral::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  // TODO sort these correctly.
  decl_order_.reserve(inputs.size());
  for (size_t i = 0; i < inputs.size(); ++i) { decl_order_.push_back(i); }
}

std::pair<FunctionLiteral *, Binding> GenericFunctionLiteral::ComputeType(
    const FnArgs<Expression *> &args, Context *ctx) {
  auto maybe_binding = Binding::MakeUntyped(this, args, lookup_);
  if (!maybe_binding.has_value()) { return std::pair(nullptr, Binding{}); }
  auto binding = std::move(maybe_binding).value();

  Context new_ctx(ctx->mod_);
  BoundConstants bound_constants;
  new_ctx.bound_constants_ = &bound_constants;
  Expression *expr_to_eval = nullptr;

  for (size_t i : decl_order_) {
    inputs[i]->VerifyType(&new_ctx);
    if (inputs[i]->type == type::Err) { return std::pair(nullptr, Binding{}); }

    if (binding.defaulted(i) && inputs[i]->IsDefaultInitialized()) {
      // TODO maybe send back an explanation of why this didn't match. Or
      // perhaps continue to get better diagnostics?
      return std::pair(nullptr, Binding{});
    } else {
      if (inputs[i]->const_ && !binding.defaulted(i) &&
          binding.exprs_[i].second->lvalue != Assign::Const) {
        return std::pair(nullptr, Binding{});
      }

      if (!binding.defaulted(i)) {
        if (auto *match =
                type::Meet(binding.exprs_[i].second->type, inputs[i]->type);
            match == nullptr) {
          return std::pair(nullptr, Binding{});
        }
      }
    }

    if (inputs[i]->const_) {
      bound_constants.emplace(
          inputs[i]->identifier->token,
          (binding.defaulted(i) ? Evaluate(inputs[i].get(), &new_ctx)
                                : Evaluate(binding.exprs_[i].second, ctx))[0]);
    }

    binding.exprs_[i].first = inputs[i]->type;
  }

  auto[iter, success] =
      fns_.emplace(std::move(bound_constants), FunctionLiteral{});
  new_ctx.bound_constants_ = &iter->first;
  if (success) {
    auto &func                 = iter->second;
    func.bound_constants_      = &iter->first;
    func.return_type_inferred_ = return_type_inferred_;

    func.inputs.reserve(inputs.size());
    for (const auto &input : inputs) {
      func.inputs.emplace_back(input->Clone());
      func.inputs.back()->arg_val = &func;
    }

    func.outputs.reserve(outputs.size());
    for (const auto &output : outputs) {
      func.outputs.emplace_back(output->Clone());
      if (output->is<Declaration>()) {
        func.outputs.back()->as<Declaration>().arg_val = &func;
      }
    }

    func.statements = base::wrap_unique(statements->Clone());
    func.ClearIdDecls();

    func.assign_scope(scope_);
    func.VerifyType(&new_ctx);
    func.Validate(&new_ctx);
    func.EmitIR(&new_ctx);

    if (new_ctx.num_errors() > 0) {
      // TODO figure out the call stack of generic function requests and then
      // print the relevant parts.
      std::cerr << "While generating code for generic function:\n";
      new_ctx.DumpErrors();
      // TODO delete all the data held by iter->second
      return std::pair(nullptr, Binding{});
    }
  }

  binding.fn_expr_ = &iter->second;
  return std::pair(&iter->second, binding);
}

void FunctionLiteral::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  for (auto &in : inputs) { in->Validate(ctx); }
  for (auto &out : outputs) { out->Validate(ctx); }

  // NOTE! Type verifcation on statements first!
  statements->VerifyType(ctx);
  HANDLE_CYCLIC_DEPENDENCIES;

  std::vector<const Expression *> rets;
  statements->ExtractReturns(&rets);
  statements->Validate(ctx);
  std::set<const type::Type *> types;
  for (auto *expr : rets) { types.insert(expr->type); }

  std::vector<const type::Type *> input_type_vec, output_type_vec;
  input_type_vec.reserve(inputs.size());
  for (const auto &input : inputs) { input_type_vec.push_back(input->type); }

  if (return_type_inferred_) {
    switch (types.size()) {
      case 0: type = type::Func(std::move(input_type_vec), {}); break;
      case 1: {
        auto *one_type = *types.begin();
        if (one_type->is<type::Tuple>()) {
          const auto &entries = one_type->as<type::Tuple>().entries_;
          for (auto *entry : entries) {
            outputs.push_back(
                std::make_unique<Terminal>(TextSpan(), IR::Val::Type(entry)));
          }
          type = type::Func(std::move(input_type_vec), entries);
        } else {
          outputs.push_back(
              std::make_unique<Terminal>(TextSpan(), IR::Val::Type(one_type)));
          type = type::Func(std::move(input_type_vec), {one_type});
        }
      } break;
      default: {
        // Note: this feels impossible, but it is possible if we allow scopes to
        // both evaluate to values and return.
        NOT_YET();
      } break;
    }
  } else {
    const auto &outs = type->as<type::Function>().output;
    switch (outs.size()) {
      case 0: {
        for (auto *expr : rets) {
          ctx->error_log_.NoReturnTypes(expr);
          limit_to(StageRange::NoEmitIR());
        }
      } break;
      case 1: {
        for (auto *expr : rets) {
          if (expr->type == outs[0]) { continue; }
          limit_to(StageRange::NoEmitIR());
          ctx->error_log_.ReturnTypeMismatch(outs[0], expr);
        }
      } break;
      default: {
        for (auto *expr : rets) {
          if (expr->type->is<type::Tuple>()) {
            const auto &tup_entries = expr->type->as<type::Tuple>().entries_;
            if (tup_entries.size() != outs.size()) {
              ctx->error_log_.ReturningWrongNumber(expr, outs.size());
              limit_to(StageRange::NoEmitIR());
            } else {
              for (size_t i = 0; i < tup_entries.size(); ++i) {
                // TODO compare with Join rather than direct comparison
                if (tup_entries AT(i) != outs AT(i)) {
                  // TODO if this is a commalist we can point to it more
                  // carefully but if we're just passing on multiple return
                  // values it's harder.
                  ctx->error_log_.IndexedReturnTypeMismatch(outs AT(i), expr,
                                                            i);
                  limit_to(StageRange::NoEmitIR());
                }
              }
            }
          } else {
            ctx->error_log_.ReturningWrongNumber(expr, outs.size());
            limit_to(StageRange::NoEmitIR());
          }
        }
      } break;
    }
  }
}

void FunctionLiteral::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  for (auto &input : inputs) { input->SaveReferences(scope, args); }
  for (auto &output : outputs) { output->SaveReferences(scope, args); }
  statements->SaveReferences(fn_scope.get(), args);
}

void GenericFunctionLiteral::SaveReferences(Scope *scope,
                                            std::vector<IR::Val> *args) {
  FunctionLiteral::SaveReferences(scope, args);
}

void FunctionLiteral::contextualize(
    const Node *correspondant,
    const std::unordered_map<const Expression *, IR::Val> &replacements) {
  for (size_t i = 0; i < inputs.size(); ++i) {
    inputs[i]->contextualize(
        correspondant->as<FunctionLiteral>().inputs[i].get(), replacements);
  }
  for (size_t i = 0; i < outputs.size(); ++i) {
    outputs[i]->contextualize(
        correspondant->as<FunctionLiteral>().outputs[i].get(), replacements);
  }

  statements->contextualize(
      correspondant->as<FunctionLiteral>().statements.get(), replacements);
}

void GenericFunctionLiteral::contextualize(
    const Node *correspondant,
    const std::unordered_map<const Expression *, IR::Val> &replacements) {
  FunctionLiteral::contextualize(correspondant, replacements);
}

void GenericFunctionLiteral::ExtractReturns(
    std::vector<const Expression *> *rets) const {
  FunctionLiteral::ExtractReturns(rets);
}

void FunctionLiteral::ExtractReturns(
    std::vector<const Expression *> *rets) const {
  for (auto &in : inputs) { in->ExtractReturns(rets); }
  for (auto &out : outputs) { out->ExtractReturns(rets); }
}

FunctionLiteral *FunctionLiteral::Clone() const {
  auto *result       = new FunctionLiteral;
  result->span       = span;
  result->statements = base::wrap_unique(statements->Clone());
  result->lookup_    = lookup_;
  result->inputs.reserve(inputs.size());
  for (const auto &input : inputs) {
    result->inputs.emplace_back(input->Clone());
  }
  for (const auto &output : outputs) {
    result->outputs.emplace_back(output->Clone());
  }
  return result;
}

GenericFunctionLiteral *GenericFunctionLiteral::Clone() const { UNREACHABLE(); }

IR::Val AST::GenericFunctionLiteral::EmitIR(Context *) {
  return IR::Val::GenFnLit(this);
}

IR::Val AST::FunctionLiteral::EmitIR(Context *ctx) {
  if (!ir_func_) {
    std::vector<std::pair<std::string, AST::Expression *>> args;
    args.reserve(inputs.size());
    for (const auto &input : inputs) {
      args.emplace_back(input->as<Declaration>().identifier->token,
                        input->as<Declaration>().init_val.get());
    }

    ir_func_ = ctx->mod_->AddFunc(this, std::move(args));
    ctx->mod_->to_complete_.push(this);
  }
  return IR::Val::FnLit(this);
}

void AST::FunctionLiteral::CompleteBody(Module *mod) {
  if (completed_) { return; }
  completed_ = true;

  Context ctx(mod);
  ctx.bound_constants_ = bound_constants_;
  statements->VerifyType(&ctx);
  statements->Validate(&ctx);
  limit_to(statements);
  stage_range_.low = EmitStage;

  if (stage_range_.high < EmitStage) { return; }
  if (type == type::Err) { return; }

  CURRENT_FUNC(ir_func_) {
    IR::BasicBlock::Current = ir_func_->entry();
    // Leave space for allocas that will come later (added to the entry
    // block).
    auto start_block        = IR::Func::Current->AddBlock();
    IR::BasicBlock::Current = start_block;

    // TODO arguments should be renumbered to not waste space on const values
    for (size_t i = 0; i < inputs.size(); ++i) {
      // TODO positional arguments
      if (inputs[i]->const_) {
        auto *val =
            AST::find(ctx.bound_constants_, inputs[i]->identifier->token);
        if (val) { inputs[i]->addr = *val; }
        continue;
      }
      inputs[i]->addr = IR::Func::Current->Argument(static_cast<i32>(i));
    }

    for (size_t i = 0; i < outputs.size(); ++i) {
      if (!outputs[i]->is<Declaration>()) { continue; }
      outputs[i]->as<Declaration>().addr =
          IR::Val::Ret(IR::ReturnValue(i), outputs[i]->type);
    }

    for (auto scope : fn_scope->innards_) {
      scope->ForEachDeclHere([](Declaration *decl) {
        if (decl->const_) {
          // Compute these values lazily in Identifier::EmitIR, because
          // otherwise we would have to figure out a valid ordering.
          return;
        }

        if (decl->arg_val) { return; }
        ASSERT(decl->type != nullptr);
        decl->addr = IR::Alloca(decl->type);
      });
    }

    statements->VerifyType(&ctx);
    statements->Validate(&ctx);
    statements->EmitIR(&ctx);
    if (type->as<type::Function>().output.empty()) {
      // TODO even this is wrong. Figure out the right jumping strategy
      // between here and where you call SetReturn
      IR::ReturnJump();
    }

    IR::BasicBlock::Current = ir_func_->entry();
    IR::UncondJump(start_block);
  }
}

IR::Val FunctionLiteral::EmitLVal(Context *) { UNREACHABLE(this); }
IR::Val GenericFunctionLiteral::EmitLVal(Context *) { UNREACHABLE(this); }
}  // namespace AST
