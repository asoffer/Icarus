#include "ast/function_literal.h"

#include <sstream>
#include "ast/bound_constants.h"
#include "ast/declaration.h"
#include "ast/stages.h"
#include "ast/terminal.h"
#include "ast/verify_macros.h"
#include "backend/eval.h"
#include "context.h"
#include "error/log.h"
#include "ir/func.h"
#include "module.h"
#include "type/pointer.h"
#include "type/function.h"
#include "type/tuple.h"
#include "type/type.h"

namespace AST {
GeneratedFunction *Function::generate(BoundConstants bc) {
  auto[iter, success] =
      fns_.emplace(std::move(bc), GeneratedFunction{});
  if (!success) { return &iter->second; }

  auto &func                 = iter->second;
  func.generated_from_       = this;
  func.bound_constants_      = &iter->first;
  func.return_type_inferred_ = return_type_inferred_;

  Context ctx(ASSERT_NOT_NULL(module_));
  ctx.bound_constants_ = func.bound_constants_;

  func.inputs.reserve(inputs.size());
  for (const auto &input : inputs) {
    // TODO this copies the type_expr on the input and then, if it's an
    // interface, we overwrite it. Easy, but that copy is unnecessary and we
    // sholud remove it.
    func.inputs.emplace_back(input->Clone());
    auto ifc_iter = ctx.bound_constants_->interfaces_.find(input.get());
    if (ifc_iter ==  ctx.bound_constants_->interfaces_.end()) {
      func.inputs.back()->arg_val = &func;
    } else {
      auto type_expr_span           = func.inputs.back()->type_expr->span;
      func.inputs.back()->type_expr = std::make_unique<Terminal>(
          type_expr_span, IR::Val::Type(ifc_iter->second));
    }
  }

  func.lookup_ = lookup_;

  func.outputs.reserve(outputs.size());
  for (const auto &output : outputs) {
    func.outputs.emplace_back(output->Clone());
    if (output->is<Declaration>()) {
      func.outputs.back()->as<Declaration>().arg_val = &func;
    }
  }

  func.statements = base::wrap_unique(statements->Clone());
  func.assign_scope(scope_);
  func.VerifyType(&ctx);
  func.Validate(&ctx);
  func.EmitIR(&ctx);

  if (ctx.num_errors() > 0) {
    // TODO figure out the call stack of generic function requests and then
    // print the relevant parts.
    std::cerr << "While generating code for generic function:\n";
    ctx.DumpErrors();
    // TODO delete all the data held by iter->second
    return nullptr;
  }
  return &func;
}

std::string FuncContent::to_string(size_t n) const {
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

void FuncContent::assign_scope(Scope *scope) {
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

void FuncContent::VerifyType(Context *ctx) {
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
    base::vector<const type::Type *> input_type_vec;
    input_type_vec.reserve(inputs.size());
    for (const auto &input : inputs) { input_type_vec.push_back(input->type); }

    // TODO should named return types be required?
    base::vector<IR::Val> out_vals;
    out_vals.reserve(outputs.size());

    for (const auto &out : outputs) {
      auto result = backend::Evaluate(
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

    base::vector<const type::Type *> ret_types;
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

void Function::VerifyType(Context *ctx) {
  bool is_generic = false;
  // TODO this loop can be decided on much earlier.
  for (const auto &input : inputs) {
    if (input->const_) {
      is_generic = true;
      break;
    }
  }

  if (is_generic) {
    VERIFY_STARTING_CHECK_EXPR;
    type   = type::Generic;
    lvalue = Assign::Const;
  } else {
    FuncContent::VerifyType(ctx);
  }
}

void Function::Validate(Context *ctx) {
  // TODO this loop can be decided on much earlier.
  for (const auto &input : inputs) {
    if (input->const_) { return; }
  }
  FuncContent::Validate(ctx);
}

void FuncContent::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  for (auto &in : inputs) { in->Validate(ctx); }
  for (auto &out : outputs) { out->Validate(ctx); }

  // NOTE! Type verifcation on statements first!
  statements->VerifyType(ctx);
  HANDLE_CYCLIC_DEPENDENCIES;

  base::vector<const Expression *> rets;
  statements->ExtractReturns(&rets);
  statements->Validate(ctx);
  std::set<const type::Type *> types;
  for (auto *expr : rets) { types.insert(expr->type); }

  base::vector<const type::Type *> input_type_vec, output_type_vec;
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
                if (tup_entries.at(i) != outs.at(i)) {
                  // TODO if this is a commalist we can point to it more
                  // carefully but if we're just passing on multiple return
                  // values it's harder.
                  ctx->error_log_.IndexedReturnTypeMismatch(outs.at(i), expr,
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

void FuncContent::SaveReferences(Scope *scope, base::vector<IR::Val> *args) {
  for (auto &input : inputs) { input->SaveReferences(scope, args); }
  for (auto &output : outputs) { output->SaveReferences(scope, args); }
  statements->SaveReferences(fn_scope.get(), args);
}

void FuncContent::contextualize(
    const Node *correspondant,
    const base::unordered_map<const Expression *, IR::Val> &replacements) {
  for (size_t i = 0; i < inputs.size(); ++i) {
    inputs[i]->contextualize(
        correspondant->as<FuncContent>().inputs[i].get(), replacements);
  }
  for (size_t i = 0; i < outputs.size(); ++i) {
    outputs[i]->contextualize(
        correspondant->as<FuncContent>().outputs[i].get(), replacements);
  }

  statements->contextualize(
      correspondant->as<FuncContent>().statements.get(), replacements);
}

void FuncContent::ExtractReturns(
    base::vector<const Expression *> *rets) const {
  for (auto &in : inputs) { in->ExtractReturns(rets); }
  for (auto &out : outputs) { out->ExtractReturns(rets); }
}

namespace {
void CloneTo(const FuncContent &from, FuncContent *to) {
  to->module_               = from.module_;
  to->span                  = from.span;
  to->statements            = base::wrap_unique(from.statements->Clone());
  to->lookup_               = from.lookup_;
  to->return_type_inferred_ = from.return_type_inferred_;
  to->inputs.reserve(from.inputs.size());
  for (const auto &input : from.inputs) {
    to->inputs.emplace_back(input->Clone());
  }
  to->outputs.reserve(from.outputs.size());
  for (const auto &output : from.outputs) {
    to->outputs.emplace_back(output->Clone());
  }
}
}  // namespace

FuncContent *FuncContent::Clone() const {
  auto *result = new FuncContent;
  CloneTo(*this, result);
  return result;
}

GeneratedFunction *GeneratedFunction::Clone() const {
  auto *result = new GeneratedFunction;
  CloneTo(*this, result);
  result->ir_func_ = nullptr;
  return result;
}

Function *Function::Clone() const {
  auto *result = new Function;
  CloneTo(*this, result);
  // TODO how to deal with cloning generated bindings?
  return result;
}

base::vector<IR::Val> Function::EmitIR(Context *ctx) {
  // TODO this loop can be decided on much earlier.
  for (const auto &input : inputs) {
    if (input->const_) { return {IR::Val::Func(this)}; }
  }

  // TODO this is a hack because I can't tell the difference between a generic
  // function early enough. This really indicates that the code structure here
  // is still wrong.
  return generate(*ctx->bound_constants_)->EmitIR(ctx);
}

base::vector<IR::Val> GeneratedFunction::EmitIR(Context *ctx) {
  if (stage_range_.high < EmitStage) { return {}; }

  if (!ir_func_) {
    base::vector<std::pair<std::string, Expression *>> args;
    args.reserve(inputs.size());
    for (const auto &input : inputs) {
      args.emplace_back(input->as<Declaration>().identifier->token,
                        input->as<Declaration>().init_val.get());
    }

    ir_func_ = ctx->mod_->AddFunc(this, std::move(args));
    ctx->mod_->to_complete_.push(this);
  }

  return {IR::Val::Func(ir_func_)};
}

void GeneratedFunction::CompleteBody(Module *mod) {
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
      outputs[i]->as<Declaration>().addr = IR::Func::Current->Return(i);
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
        if (decl->addr == IR::Val::None()) {
          // TODO this test isn't perfect and needs to change (c.f. const
          // arguments?)
          decl->addr =
              IR::Val::Reg(IR::Alloca(decl->type), type::Ptr(decl->type));
        }
      });
    }

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

base::vector<IR::Val> GeneratedFunction::EmitLVal(Context *) { UNREACHABLE(this); }
base::vector<IR::Val> Function::EmitLVal(Context *) { UNREACHABLE(this); }
}  // namespace AST
