#include "ast/function_literal.h"

#include <sstream>

#include "absl/container/flat_hash_set.h"
#include "ast/declaration.h"
#include "ast/match_declaration.h"
#include "ast/repeated_unop.h"
#include "ast/terminal.h"
#include "backend/eval.h"
#include "error/log.h"
#include "ir/func.h"
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

void FunctionLiteral::assign_scope(core::Scope *scope) {
  scope_ = scope;
  if (!fn_scope_) {  // TODO can this ever be null?
    fn_scope_          = scope->add_child<core::FnScope>();
    fn_scope_->fn_lit_ = this;
  }

  for (auto &in : inputs_) { in.value->assign_scope(fn_scope_.get()); }
  for (auto &out : outputs_) { out->assign_scope(fn_scope_.get()); }
  statements_.assign_scope(fn_scope_.get());

  DeclDepGraph decl_dep_graph;
  for (auto const &in : inputs_) {
    decl_dep_graph.graph_.add_node(in.value.get());
    if (in.value->type_expr) {
      in.value->type_expr->DependentDecls(&decl_dep_graph, in.value.get());
    }
    if (in.value->init_val) {
      in.value->init_val->DependentDecls(&decl_dep_graph, in.value.get());
    }
  }

  absl::flat_hash_map<std::string_view, Declaration*> decls_by_id;
  for (auto const& param : inputs_) {
    decls_by_id.emplace(param.value->id_, param.value.get());
  }

  param_dep_graph_ = std::move(decl_dep_graph.graph_);
  for (auto &[id, decls] : decl_dep_graph.ids_) {
    auto iter = decls_by_id.find(id);
    if (iter == decls_by_id.end()) { continue; }
    for (auto *d : decls) { param_dep_graph_.add_edge(d, iter->second); }
  }

  sorted_params_.reserve(param_dep_graph_.num_nodes());
  param_dep_graph_.topologically(
      [this](Declaration *decl) { sorted_params_.push_back(decl); });
  for (size_t i = 0; i < inputs_.size(); ++i) {
    decl_to_param_.emplace(inputs_.at(i).value.get(), i);
  }
}

void FunctionLiteral::DependentDecls(DeclDepGraph *g, Declaration *d) const {
  for (auto const &in : inputs_) { in.value->DependentDecls(g, d); }
  for (auto const &out : outputs_) { out->DependentDecls(g, d); }
}

VerifyResult FunctionLiteral::VerifyType(Context *ctx) {
  for (auto const &p : inputs_) {
    if (p.value->const_ || !param_dep_graph_.at(p.value.get()).empty()) {
      return ctx->set_result(this, VerifyResult::Constant(type::Generic));
    }
  }

  return VerifyTypeConcrete(ctx);
}

VerifyResult FunctionLiteral::VerifyTypeConcrete(Context *ctx) {
  std::vector<type::Type const *> input_type_vec;
  input_type_vec.reserve(inputs_.size());
  for (auto &d : inputs_) {
    ASSIGN_OR(return _, auto result, d.value->VerifyType(ctx));
    input_type_vec.push_back(result.type_);
  }

  std::vector<type::Type const *> output_type_vec;
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
  if (ctx->num_errors() > 0) { return VerifyResult::Error(); }

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

    ctx->mod_->deferred_work_.emplace(
        [constants{ctx->constants_}, this, mod{ctx->mod_}]() mutable {
          Context ctx(mod);
          ctx.constants_ = constants;
          this->VerifyBody(&ctx);
        });

    return ctx->set_result(
        this, VerifyResult::Constant(type::Func(std::move(input_type_vec),
                                                std::move(output_type_vec))));
  } else {
    return VerifyBody(ctx);
  }
}

// TODO there's not that much shared between the inferred and uninferred cases,
// so probably break them out.
VerifyResult FunctionLiteral::VerifyBody(Context *ctx) {
  statements_.VerifyType(ctx);
  // TODO propogate cyclic dependencies.

  JumpExprs rets;
  statements_.ExtractJumps(&rets);
  absl::flat_hash_set<type::Type const *> types;
  for (auto *expr : rets[JumpKind::Return]) {
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
    return ctx->set_result(this, VerifyResult::Constant(f));

  } else {
    auto *this_type  = ctx->type_of(this);
    auto const &outs = ASSERT_NOT_NULL(this_type)->as<type::Function>().output;
    switch (outs.size()) {
      case 0: {
        bool err = false;
        for (auto *expr : rets[JumpKind::Return]) {
          if (!expr->as<CommaList>().exprs_.empty()) {
            ctx->error_log()->NoReturnTypes(expr);
            err = true;
          }
        }
        return err ? VerifyResult::Error() : VerifyResult::Constant(this_type);
      } break;
      case 1: {
        bool err = false;
        for (auto *expr : rets[JumpKind::Return]) {
          auto *t = ASSERT_NOT_NULL(ctx->type_of(expr));
          if (t == outs[0]) { continue; }
          ctx->error_log()->ReturnTypeMismatch(outs[0], t, expr->span);
          err = true;
        }
        return err ? VerifyResult::Error() : VerifyResult::Constant(this_type);
      } break;
      default: {
        for (auto *expr : rets[JumpKind::Return]) {
          auto *expr_type = ctx->type_of(expr);
          if (expr_type->is<type::Tuple>()) {
            auto const &tup_entries = expr_type->as<type::Tuple>().entries_;
            if (tup_entries.size() != outs.size()) {
              ctx->error_log()->ReturningWrongNumber(expr->span, expr_type,
                                                     outs.size());
              return VerifyResult::Error();
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
              if (err) { return VerifyResult::Error(); }
            }
          } else {
            ctx->error_log()->ReturningWrongNumber(expr->span, expr_type,
                                                   outs.size());
            return VerifyResult::Error();
          }
        }
        return VerifyResult::Constant(this_type);
      } break;
    }
  }
}

void FunctionLiteral::ExtractJumps(JumpExprs *rets) const {
  for (auto &in : inputs_) { in.value->ExtractJumps(rets); }
  for (auto &out : outputs_) { out->ExtractJumps(rets); }
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
  ir::Func *&ir_func = ctx->constants_->second.ir_funcs_[this];
  if (!ir_func) {
    auto &work_item = ctx->mod_->deferred_work_.emplace(
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
              return type::Typed(e->init_val.get(), fn_type->input.at(i++));
            }));
    ir_func->work_item = &work_item;
  }

  return ir::Results{ir_func};
}

void FunctionLiteral::CompleteBody(Context *ctx) {
  // TODO have validate return a bool distinguishing if there are errors and
  // whether or not we can proceed.

  auto *t = ctx->type_of(this);

  ir::Func *&ir_func = ctx->constants_->second.ir_funcs_[this];

  CURRENT_FUNC(ir_func) {
    ir::BasicBlock::Current = ir_func->entry();
    // Leave space for allocas that will come later (added to the entry
    // block).
    auto start_block        = ir::Func::Current->AddBlock();
    ir::BasicBlock::Current = start_block;

    // TODO arguments should be renumbered to not waste space on const values
    for (int32_t i = 0; i < static_cast<int32_t>(inputs_.size()); ++i) {
      ctx->set_addr(inputs_.at(i).value.get(), ir::Func::Current->Argument(i));
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
