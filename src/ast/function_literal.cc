#include "ast/function_literal.h"

#include <sstream>
#include "ast/bound_constants.h"
#include "ast/declaration.h"
#include "ast/terminal.h"
#include "backend/eval.h"
#include "context.h"
#include "error/log.h"
#include "ir/func.h"
#include "module.h"
#include "type/function.h"
#include "type/tuple.h"
#include "type/type.h"

namespace ast {
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

void FunctionLiteral::assign_scope(Scope *scope) {
  scope_ = scope;
  if (!fn_scope) { // TODO can this ever be null? 
    fn_scope          = scope->add_child<FnScope>();
    fn_scope->fn_lit_ = this;
  }
  for (auto &in : inputs) { in->assign_scope(fn_scope.get()); }
  for (auto &out : outputs) { out->assign_scope(fn_scope.get()); }
  statements->assign_scope(fn_scope.get());
}

type::Type const *FunctionLiteral::VerifyType(Context *ctx) {
  if (std::any_of(inputs.begin(), inputs.end(),
                  [](auto const &decl) { return decl->const_; })) {
    return ctx->set_type(this, type::Generic);
  }
  return VerifyTypeConcrete(ctx);
}

type::Type const *FunctionLiteral::VerifyTypeConcrete(Context *ctx) {
  base::vector<const type::Type *> input_type_vec;
  input_type_vec.reserve(inputs.size());
  for (auto &input : inputs) {
    input_type_vec.push_back(input->VerifyType(ctx));
  }

  base::vector<const type::Type *> output_type_vec;
  output_type_vec.reserve(outputs.size());
  for (auto &output : outputs) {
    output_type_vec.push_back(output->VerifyType(ctx));
  }

  if (std::any_of(input_type_vec.begin(), input_type_vec.end(),
                  [](type::Type const *t) { return t == nullptr; }) ||
      std::any_of(output_type_vec.begin(), output_type_vec.end(),
                  [](type::Type const *t) { return t == nullptr; })) {
    return nullptr;
  }

  // TODO need a better way to say if there was an error recorded in a
  // particular section of compilation. Right now we just have the grad total
  // count.
  if (ctx->num_errors() > 0) { return nullptr; }

  if (!return_type_inferred_) {
    for (size_t i = 0; i < output_type_vec.size(); ++i) {
      if (outputs.at(i)->is<Declaration>()) {
        output_type_vec.at(i) = ctx->type_of(&outputs[i]->as<Declaration>());
      } else {
        ASSERT(output_type_vec.at(i) == type::Type_);
        output_type_vec.at(i) =
            backend::EvaluateAs<type::Type const *>(outputs.at(i).get(), ctx);
      }
    }

    return ctx->set_type(this, type::Func(std::move(input_type_vec),
                                          std::move(output_type_vec)));
  } else {
    Validate(ctx);
    return ctx->type_of(this);
  }
}

// TODO VerifyType has access to types of previous entries, but Validate
// doesnt.
void FunctionLiteral::Validate(Context *ctx) {
  if (ctx->mod_->type_of(ctx->bound_constants_, this) == type::Generic) {
    return;
  }

  auto &validated_fns = ctx->mod_->validated_[ctx->bound_constants_];
  bool inserted = validated_fns.insert(this).second;
  if (!inserted) { return; }

  for (auto &in : inputs) { in->Validate(ctx); }
  for (auto &out : outputs) { out->Validate(ctx); }

  // NOTE! Type verifcation on statements first!
  statements->VerifyType(ctx);
  // TODO propogate cyclic dependencies.

  JumpExprs rets;
  statements->ExtractJumps(&rets);
  statements->Validate(ctx);
  std::set<type::Type const *> types;
  for (auto *expr : rets[JumpKind::Return]) { types.insert(ctx->type_of(expr)); }

  base::vector<type::Type const *> input_type_vec;
  input_type_vec.reserve(inputs.size());
  for (auto const &input : inputs) {
    input_type_vec.push_back(ASSERT_NOT_NULL(ctx->type_of(input.get())));
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
            outputs.push_back(
                std::make_unique<Terminal>(TextSpan(), ir::Val(entry)));
          }
          ctx->set_type(this, type::Func(std::move(input_type_vec), entries));

        } else {
          outputs.push_back(
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
          ctx->error_log_.NoReturnTypes(expr);
        }
      } break;
      case 1: {
        for (auto *expr : rets[JumpKind::Return]) {
          if (ctx->type_of(expr) == outs[0]) { continue; }
          ctx->error_log_.ReturnTypeMismatch(outs[0], expr);
        }
      } break;
      default: {
        for (auto *expr : rets[JumpKind::Return]) {
          auto *expr_type = ctx->type_of(expr);
          if (expr_type->is<type::Tuple>()) {
            auto const &tup_entries = expr_type->as<type::Tuple>().entries_;
            if (tup_entries.size() != outs.size()) {
              ctx->error_log_.ReturningWrongNumber(expr, outs.size());
            } else {
              for (size_t i = 0; i < tup_entries.size(); ++i) {
                // TODO compare with Join rather than direct comparison
                if (tup_entries.at(i) != outs.at(i)) {
                  // TODO if this is a commalist we can point to it more
                  // carefully but if we're just passing on multiple return
                  // values it's harder.
                  ctx->error_log_.IndexedReturnTypeMismatch(outs.at(i), expr,
                                                            i);
                }
              }
            }
          } else {
            ctx->error_log_.ReturningWrongNumber(expr, outs.size());
          }
        }
      } break;
    }
  }
}

void FunctionLiteral::ExtractJumps(JumpExprs *rets) const {
  for (auto &in : inputs) { in->ExtractJumps(rets); }
  for (auto &out : outputs) { out->ExtractJumps(rets); }
}

base::vector<ir::Val> FunctionLiteral::EmitIR(Context *ctx) {
  if (std::any_of(inputs.begin(), inputs.end(), [&](auto const &decl) {
        return decl->const_ &&
               ctx->bound_constants_.constants_.find(decl.get()) ==
                   ctx->bound_constants_.constants_.end();
      })) {
    return {ir::Val::Func(this)};
  }

  ir::Func *&ir_func = ctx->mod_->ir_funcs_[ctx->bound_constants_][this];
  if (!ir_func) {
    auto &work_item =
        ctx->mod_->to_complete_.emplace(ctx->bound_constants_, this, ctx->mod_);

    base::vector<std::pair<std::string, Expression *>> args;
    args.reserve(inputs.size());
    for (const auto &input : inputs) {
      args.emplace_back(input->as<Declaration>().id_,
                        input->as<Declaration>().init_val.get());
    }

    ir_func = ctx->mod_->AddFunc(&ctx->type_of(this)->as<type::Function>(),
                                 std::move(args));
    ir_func->work_item = &work_item;
  }

  return {ir::Val::Func(ir_func->type_, ir_func)};
}

void FunctionLiteral::CompleteBody(Context *ctx) {
  // TODO have validate return a bool distinguishing if there are errors and
  // whether or not we can proceed.

  auto *t = ctx->type_of(this);

  ir::Func *&ir_func = ctx->mod_->ir_funcs_[ctx->bound_constants_][this];
  CURRENT_FUNC(ir_func) {
    ir::BasicBlock::Current = ir_func->entry();
    // Leave space for allocas that will come later (added to the entry
    // block).
    auto start_block        = ir::Func::Current->AddBlock();
    ir::BasicBlock::Current = start_block;

    // TODO arguments should be renumbered to not waste space on const values
    for (i32 i = 0; i < static_cast<i32>(inputs.size()); ++i) {
      ctx->set_addr(inputs[i].get(), ir::Func::Current->Argument(i));
    }

    fn_scope->MakeAllStackAllocations(ctx);

    for (size_t i = 0; i < outputs.size(); ++i) {
      if (!outputs[i]->is<Declaration>()) { continue; }
      auto *out_decl      = &outputs[i]->as<Declaration>();
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

    statements->EmitIR(ctx);

    fn_scope->MakeAllDestructions(ctx);


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
