#include "ast/function_literal.h"

#include <sstream>

#include "absl/container/flat_hash_set.h"
#include "ast/declaration.h"
#include "ast/match_declaration.h"
#include "ast/repeated_unop.h"
#include "ast/terminal.h"
#include "backend/eval.h"
#include "error/log.h"
#include "ir/compiled_fn.h"
#include "misc/context.h"
#include "misc/module.h"
#include "type/function.h"
#include "type/tuple.h"
#include "type/type.h"

namespace ast {
std::string FunctionLiteral::to_string(size_t n) const {
  std::stringstream ss;
  ss << "(";
  if (!inputs_.empty()) {
    auto iter = inputs_.begin();
    ss << iter->value->to_string(n);
    ++iter;
    while (iter != inputs_.end()) {
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

// TODO there's not that much shared between the inferred and uninferred cases,
// so probably break them out.
ast_visitor::VerifyResult FunctionLiteral::VerifyBody(
    ast_visitor::VerifyType const *visitor, Context *ctx) const {
  statements_.VerifyType(visitor, ctx);
  // TODO propogate cyclic dependencies.

  ast_visitor::ExtractJumps extract_visitor;
  statements_.ExtractJumps(&extract_visitor);

  // TODO we can have yields and returns, or yields and jumps, but not jumps and
  // returns. Check this.
  absl::flat_hash_set<type::Type const *> types;
  for (auto *expr :
       extract_visitor.exprs(ast_visitor::ExtractJumps::Kind::Return)) {
    types.insert(ctx->type_of(expr));
  }

  if (return_type_inferred_) {
    std::vector<type::Type const *> input_type_vec;
    input_type_vec.reserve(inputs_.size());
    for (auto &input : inputs_) {
      input_type_vec.push_back(
          ASSERT_NOT_NULL(ctx->type_of(input.value.get())));
    }

    std::vector<type::Type const *> output_type_vec(
        std::make_move_iterator(types.begin()),
        std::make_move_iterator(types.end()));

    if (types.size() > 1) { NOT_YET("log an error"); }
    auto f = type::Func(std::move(input_type_vec), std::move(output_type_vec));
    return ctx->set_result(this, ast_visitor::VerifyResult::Constant(f));

  } else {
    auto *this_type  = ctx->type_of(this);
    auto const &outs = ASSERT_NOT_NULL(this_type)->as<type::Function>().output;
    switch (outs.size()) {
      case 0: {
        bool err = false;
        for (auto *expr :
             extract_visitor.exprs(ast_visitor::ExtractJumps::Kind::Return)) {
          base::Log() << expr->to_string(0);
          if (!expr->as<ast::CommaList>().exprs_.empty()) {
            ctx->error_log()->NoReturnTypes(expr);
            err = true;
          }
        }
        return err ? ast_visitor::VerifyResult::Error()
                   : ast_visitor::VerifyResult::Constant(this_type);
      } break;
      case 1: {
        bool err = false;
        for (auto *expr :
             extract_visitor.exprs(ast_visitor::ExtractJumps::Kind::Return)) {
          auto *t = ASSERT_NOT_NULL(ctx->type_of(expr));
          if (t == outs[0]) { continue; }
          ctx->error_log()->ReturnTypeMismatch(outs[0], t, expr->span);
          err = true;
        }
        return err ? ast_visitor::VerifyResult::Error()
                   : ast_visitor::VerifyResult::Constant(this_type);
      } break;
      default: {
        for (auto *expr :
             extract_visitor.exprs(ast_visitor::ExtractJumps::Kind::Return)) {
          auto *expr_type = ctx->type_of(expr);
          if (expr_type->is<type::Tuple>()) {
            auto const &tup_entries = expr_type->as<type::Tuple>().entries_;
            if (tup_entries.size() != outs.size()) {
              ctx->error_log()->ReturningWrongNumber(expr->span, expr_type,
                                                     outs.size());
              return ast_visitor::VerifyResult::Error();
            } else {
              bool err = false;
              for (size_t i = 0; i < tup_entries.size(); ++i) {
                if (tup_entries.at(i) != outs.at(i)) {
                  // TODO if this is a commalist we can point to it more
                  // carefully but if we're just passing on multiple return
                  // values it's harder.
                  //
                  // TODO point the span to the correct entry which may be hard
                  // if it's splatted.
                  ctx->error_log()->IndexedReturnTypeMismatch(
                      outs.at(i), tup_entries.at(i), expr->span, i);
                  err = true;
                }
              }
              if (err) { return ast_visitor::VerifyResult::Error(); }
            }
          } else {
            ctx->error_log()->ReturningWrongNumber(expr->span, expr_type,
                                                   outs.size());
            return ast_visitor::VerifyResult::Error();
          }
        }
        return ast_visitor::VerifyResult::Constant(this_type);
      } break;
    }
  }
}

ast_visitor::VerifyResult FunctionLiteral::VerifyTypeConcrete(
    ast_visitor::VerifyType const *visitor, Context *ctx) const {
  std::vector<type::Type const *> input_type_vec;
  input_type_vec.reserve(inputs_.size());
  for (auto &d : inputs_) {
    ASSIGN_OR(return _, auto result, d.value->VerifyType(visitor, ctx));
    input_type_vec.push_back(result.type_);
  }

  std::vector<type::Type const *> output_type_vec;
  output_type_vec.reserve(outputs_.size());
  bool error = false;
  for (auto &output : outputs_) {
    auto result = output->VerifyType(visitor, ctx);
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
    return ast_visitor::VerifyResult::Error();
  }

  // TODO need a better way to say if there was an error recorded in a
  // particular section of compilation. Right now we just have the grad total
  // count.
  if (ctx->num_errors() > 0) { return ast_visitor::VerifyResult::Error(); }

  if (!return_type_inferred_) {
    for (size_t i = 0; i < output_type_vec.size(); ++i) {
      if (auto *decl = outputs_.at(i)->if_as<ast::Declaration>()) {
        output_type_vec.at(i) = ctx->type_of(decl);
      } else {
        ASSERT(output_type_vec.at(i) == type::Type_);
        output_type_vec.at(i) =
            backend::EvaluateAs<type::Type const *>(outputs_.at(i).get(), ctx);
      }
    }

    ctx->mod_->deferred_work_.emplace(
        [constants{ctx->constants_}, this, visitor, mod{ctx->mod_}]() mutable {
          Context ctx(mod);
          ctx.constants_ = constants;
          VerifyBody(visitor, &ctx);
        });

    return ctx->set_result(
        this, ast_visitor::VerifyResult::Constant(type::Func(
                  std::move(input_type_vec), std::move(output_type_vec))));
  } else {
    return VerifyBody(visitor, ctx);
  }
}

ir::Results FunctionLiteral::EmitIr(Context *ctx) {
  for (auto const &param : inputs_) {
    auto *p = param.value.get();
    if (p->const_ && !ctx->constants_->first.contains(p)) {
      return ir::Results{this};
    }

    for (auto *dep : param_dep_graph_.sink_deps(param.value.get())) {
      if (!ctx->constants_->first.contains(dep)) { return ir::Results{this}; }
    }
  }

  // TODO Use correct constants
  ir::CompiledFn *&ir_func = ctx->constants_->second.ir_funcs_[this];
  if (!ir_func) {
    std::function<void()> *work_item_ptr = nullptr;
    work_item_ptr                        = &ctx->mod_->deferred_work_.emplace(
        [constants{ctx->constants_}, this, mod{ctx->mod_}]() mutable {
          Context ctx(mod);
          ctx.constants_ = constants;
          CompleteBody(&ctx);
        });

    auto *fn_type = &ctx->type_of(this)->as<type::Function>();

    ir_func = ctx->mod_->AddFunc(
        fn_type,
        inputs_.Transform(
            [fn_type, i = 0](std::unique_ptr<Declaration> const &e) mutable {
              return type::Typed<Expression const *>(e->init_val.get(),
                                                     fn_type->input.at(i++));
            }));
    if (work_item_ptr) { ir_func->work_item = work_item_ptr; }
  }

  return ir::Results{ir_func};
}

void FunctionLiteral::CompleteBody(Context *ctx) {
  // TODO have validate return a bool distinguishing if there are errors and
  // whether or not we can proceed.

  auto *t = ctx->type_of(this);

  ir::CompiledFn *&ir_func = ctx->constants_->second.ir_funcs_[this];

  CURRENT_FUNC(ir_func) {
    ir::BasicBlock::Current = ir_func->entry();
    // Leave space for allocas that will come later (added to the entry
    // block).
    auto start_block        = ir::CompiledFn::Current->AddBlock();
    ir::BasicBlock::Current = start_block;

    // TODO arguments should be renumbered to not waste space on const values
    for (int32_t i = 0; i < static_cast<int32_t>(inputs_.size()); ++i) {
      ctx->set_addr(inputs_.at(i).value.get(), ir::Reg::Arg(i));
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
        out_decl_type->EmitCopyAssign(
            out_decl_type, out_decl->init_val->EmitIr(ctx), alloc, ctx);
      }
    }

    statements_.EmitIr(ctx);

    fn_scope_->MakeAllDestructions(ctx);

    if (t->as<type::Function>().output.empty()) {
      // TODO even this is wrong. Figure out the right jumping strategy
      // between here and where you call SetReturn
      ir::ReturnJump();
    }

    ir::BasicBlock::Current = ir_func->entry();
    ir::UncondJump(start_block);
    ir_func->work_item = nullptr;
    std::stringstream ss;
  }
}

}  // namespace ast
