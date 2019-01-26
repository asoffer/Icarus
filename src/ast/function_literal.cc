#include "ast/function_literal.h"

#include <sstream>
#include "ast/bound_constants.h"
#include "ast/declaration.h"
#include "ast/match_declaration.h"
#include "ast/terminal.h"
#include "backend/eval.h"
#include "context.h"
#include "error/log.h"
#include "ir/func.h"
#include "module.h"
#include "repeated_unop.h"
#include "type/function.h"
#include "type/tuple.h"
#include "type/type.h"

namespace ast {
std::string FunctionLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << "(";
  if (!inputs_.empty()) {
    auto iter = inputs_.params_.begin();
    ss << iter->value->to_string(n);
    ++iter;
    while (iter != inputs_.params_.end()) {
      ss << ", " << iter->value->to_string(n);
      ++iter;
    }
  }
  ss << ") -> ";
  if (!return_type_inferred_) {
    ss << "(";
    if (!outputs_.empty()) {
      auto iter = outputs_.begin();
      ss << iter->get()->to_string(n);
      ++iter;
      while (iter != outputs_.end()) {
        ss << ", " << iter->get()->to_string(n);
        ++iter;
      }
    }
    ss << ")";
  }
  ss << " {\n"
     << statements_.to_string(n + 1) << std::string(2 * n, ' ') << "}";
  return ss.str();
}

void FunctionLiteral::assign_scope(Scope *scope) {
  scope_ = scope;
  if (!fn_scope_) { // TODO can this ever be null? 
    fn_scope_          = scope->add_child<FnScope>();
    fn_scope_->fn_lit_ = this;
  }
  for (auto &in : inputs_.params_) { in.value->assign_scope(fn_scope_.get()); }
  for (auto &out : outputs_) { out->assign_scope(fn_scope_.get()); }
  statements_.assign_scope(fn_scope_.get());
}

VerifyResult FunctionLiteral::VerifyType(Context *ctx) {
  if (std::any_of(
          inputs_.params_.begin(), inputs_.params_.end(), [](auto const &decl) {
            return decl.value->const_ ||
                   (decl.value->type_expr != nullptr &&
                    decl.value->type_expr->template is<MatchDeclaration>());
          })) {
    return VerifyResult::Constant(ctx->set_type(this, type::Generic));
  }
  return VerifyTypeConcrete(ctx);
}

VerifyResult FunctionLiteral::VerifyTypeConcrete(Context *ctx) {
  base::vector<const type::Type *> input_type_vec;
  input_type_vec.reserve(inputs_.size());
  for (auto &input : inputs_.params_) {
    input_type_vec.push_back(input.value->VerifyType(ctx).type_);
  }

  base::vector<const type::Type *> output_type_vec;
  output_type_vec.reserve(outputs_.size());
  bool error = false;
  for (auto &output : outputs_) {
    auto result = output->VerifyType(ctx);
    output_type_vec.push_back(result.type_);
    if (result.type_ != nullptr && !result.const_) {
      // TODO this feels wrong because output could be a decl. And that decl
      // being a const decl isn't what I care about.
      NOT_YET("log an error");
      error = true;
    }
  }

  if (error ||
      std::any_of(input_type_vec.begin(), input_type_vec.end(),
                  [](type::Type const *t) { return t == nullptr; }) ||
      std::any_of(output_type_vec.begin(), output_type_vec.end(),
                  [](type::Type const *t) { return t == nullptr; })) {
    return VerifyResult::Error();
  }

  // TODO need a better way to say if there was an error recorded in a
  // particular section of compilation. Right now we just have the grad total
  // count.
  if (ctx->num_errors() > 0) {
    return VerifyResult::Error(); }

  if (!return_type_inferred_) {
    for (size_t i = 0; i < output_type_vec.size(); ++i) {
      if (outputs_.at(i)->is<Declaration>()) {
        output_type_vec.at(i) = ctx->type_of(&outputs_[i]->as<Declaration>());
      } else {
        ASSERT(output_type_vec.at(i) == type::Type_);
        output_type_vec.at(i) =
            backend::EvaluateAs<type::Type const *>(outputs_.at(i).get(), ctx);
      }
    }

    return VerifyResult::Constant(ctx->set_type(
        this,
        type::Func(std::move(input_type_vec), std::move(output_type_vec))));
  } else {
    Validate(ctx);
    return VerifyResult::Constant(ctx->type_of(this));
  }
}

// TODO VerifyType has access to types of previous entries, but Validate
// doesnt.
void FunctionLiteral::Validate(Context *ctx) {
  if (ctx->mod_->type_of(ctx->bound_constants_, this) == type::Generic) {
    return;
  }

  auto &validated_fns = ctx->mod_->data_[ctx->bound_constants_].validated_;
  bool inserted = validated_fns.insert(this).second;
  if (!inserted) { return; }

  for (auto &in : inputs_.params_) { in.value->Validate(ctx); }
  for (auto &out : outputs_) { out->Validate(ctx); }

  // NOTE! Type verifcation on statements first!
  statements_.VerifyType(ctx);
  // TODO propogate cyclic dependencies.

  JumpExprs rets;
  statements_.ExtractJumps(&rets);
  statements_.Validate(ctx);
  std::set<type::Type const *> types;
  for (auto *expr : rets[JumpKind::Return]) { types.insert(ctx->type_of(expr)); }

  base::vector<type::Type const *> input_type_vec;
  input_type_vec.reserve(inputs_.size());
  for (auto const &input : inputs_.params_) {
    input_type_vec.push_back(ASSERT_NOT_NULL(ctx->type_of(input.value.get())));
  }

  if (return_type_inferred_) {
    switch (types.size()) {
      case 0:
        ctx->set_type(this, type::Func(std::move(input_type_vec), {}));
        break;
      case 1: {
        auto *one_type = *types.begin();
        if (one_type->is<type::Tuple>()) {
          const auto &entries = one_type->as<type::Tuple>().entries_;
          for (auto *entry : entries) {
            outputs_.push_back(
                std::make_unique<Terminal>(TextSpan(), ir::Val(entry)));
          }
          ctx->set_type(this, type::Func(std::move(input_type_vec), entries));

        } else {
          outputs_.push_back(
              std::make_unique<Terminal>(TextSpan(), ir::Val(one_type)));
          ctx->set_type(this,
                        type::Func(std::move(input_type_vec), {one_type}));
        }
      } break;
      default: {
        // Note: this feels impossible, but it is possible if we allow scopes to
        // both evaluate to values and return.
        NOT_YET();
      } break;
    }
  } else {
    auto const &outs =
        ASSERT_NOT_NULL(ctx->type_of(this))->as<type::Function>().output;
    switch (outs.size()) {
      case 0: {
        for (auto *expr : rets[JumpKind::Return]) {
          if (!expr->as<CommaList>().exprs_.empty()) {
            ctx->error_log_.NoReturnTypes(expr);
          }
        }
      } break;
      case 1: {
        for (auto *expr : rets[JumpKind::Return]) {
          auto *t = ctx->type_of(expr);
          if (t == outs[0]) { continue; }
          ctx->error_log_.ReturnTypeMismatch(outs[0], t, expr->span);
        }
      } break;
      default: {
        for (auto *expr : rets[JumpKind::Return]) {
          auto *expr_type = ctx->type_of(expr);
          if (expr_type->is<type::Tuple>()) {
            auto const &tup_entries = expr_type->as<type::Tuple>().entries_;
            if (tup_entries.size() != outs.size()) {
              ctx->error_log_.ReturningWrongNumber(expr->span, expr_type, outs.size());
            } else {
              for (size_t i = 0; i < tup_entries.size(); ++i) {
                if (tup_entries.at(i) != outs.at(i)) {
                  // TODO if this is a commalist we can point to it more
                  // carefully but if we're just passing on multiple return
                  // values it's harder.
                  //
                  // TODO point the span to the correct entry which may be hard
                  // if it's splatted.
                  ctx->error_log_.IndexedReturnTypeMismatch(
                      outs.at(i), tup_entries.at(i), expr->span, i);
                }
              }
            }
          } else {
            ctx->error_log_.ReturningWrongNumber(expr->span, expr_type,
                                                 outs.size());
          }
        }
      } break;
    }
  }
}

void FunctionLiteral::ExtractJumps(JumpExprs *rets) const {
  for (auto &in : inputs_.params_) { in.value->ExtractJumps(rets); }
  for (auto &out : outputs_) { out->ExtractJumps(rets); }
}

base::vector<ir::Val> FunctionLiteral::EmitIR(Context *ctx) {
  if (std::any_of(
          inputs_.params_.begin(), inputs_.params_.end(),
          [&](auto const &decl) {
            // TODO this is wrong... it may not directly be a match-decl, but
            // something that's "extractable" like a pointer to or an array of
            // match-decls.
            return (
                (decl.value->const_ &&
                 ctx->bound_constants_.constants_.find(decl.value.get()) ==
                     ctx->bound_constants_.constants_.end()) ||
                (decl.value->type_expr != nullptr &&
                 decl.value->type_expr->template is<MatchDeclaration>() &&
                 ctx->bound_constants_.constants_.find(
                     &decl.value->type_expr->template as<MatchDeclaration>()) ==
                     ctx->bound_constants_.constants_.end()));
          })) {
    return {ir::Val::Func(this)};
  }

  ir::Func *&ir_func = ctx->mod_->data_[ctx->bound_constants_].ir_funcs_[this];
  if (!ir_func) {
    auto &work_item =
        ctx->mod_->to_complete_.emplace(ctx->bound_constants_, this, ctx->mod_);

    FnParams<Expression*> params;
    params.reserve(inputs_.size());
    for (auto const &input : inputs_.params_) {
      params.append(input.name, input.value->init_val.get());
    }

    ir_func = ctx->mod_->AddFunc(&ctx->type_of(this)->as<type::Function>(),
                                 std::move(params));
    ir_func->work_item = &work_item;
  }

  return {ir::Val::Func(ir_func->type_, ir_func)};
}

void FunctionLiteral::CompleteBody(Context *ctx) {
  // TODO have validate return a bool distinguishing if there are errors and
  // whether or not we can proceed.

  auto *t = ctx->type_of(this);

  ir::Func *&ir_func = ctx->mod_->data_[ctx->bound_constants_].ir_funcs_[this];
  CURRENT_FUNC(ir_func) {
    ir::BasicBlock::Current = ir_func->entry();
    // Leave space for allocas that will come later (added to the entry
    // block).
    auto start_block        = ir::Func::Current->AddBlock();
    ir::BasicBlock::Current = start_block;

    // TODO arguments should be renumbered to not waste space on const values
    for (i32 i = 0; i < static_cast<i32>(inputs_.size()); ++i) {
      ctx->set_addr(inputs_.params_.at(i).value.get(),
                    ir::Func::Current->Argument(i));
    }

    fn_scope_->MakeAllStackAllocations(ctx);

    for (size_t i = 0; i < outputs_.size(); ++i) {
      if (!outputs_[i]->is<Declaration>()) { continue; }
      auto *out_decl      = &outputs_[i]->as<Declaration>();
      auto *out_decl_type = ASSERT_NOT_NULL(ctx->type_of(out_decl));
      auto alloc = out_decl_type->is_big() ? ir::GetRet(i, out_decl_type)
                                           : ir::Alloca(out_decl_type);

      ctx->set_addr(out_decl, alloc);
      if (out_decl->IsDefaultInitialized()) {
        out_decl_type->EmitInit(alloc, ctx);
      } else {
        out_decl_type->EmitAssign(
            out_decl_type, out_decl->init_val->EmitIR(ctx)[0], alloc, ctx);
      }
    }

    statements_.EmitIR(ctx);

    fn_scope_->MakeAllDestructions(ctx);


    if (t->as<type::Function>().output.empty()) {
      // TODO even this is wrong. Figure out the right jumping strategy
      // between here and where you call SetReturn
      ir::ReturnJump();
    }

    ir::BasicBlock::Current = ir_func->entry();
    ir::UncondJump(start_block);
    ir_func->work_item = nullptr;
  }
}

base::vector<ir::RegisterOr<ir::Addr>> FunctionLiteral::EmitLVal(Context *) {
  UNREACHABLE(this);
}
}  // namespace ast
