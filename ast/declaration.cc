#include "ast/declaration.h"

#include <sstream>
#include "ast/function_literal.h"
#include "backend/eval.h"
#include "ir/compiled_fn.h"
#include "misc/module.h"
#include "type/cast.h"
#include "type/typed_value.h"

namespace ast {

std::string Declaration::to_string(size_t n) const {
  std::stringstream ss;
  ss << id_;
  if (type_expr) {
    ss << (const_ ? " :: " : ": ") << type_expr->to_string(n);
    if (init_val) { ss << " = " << init_val->to_string(n); }
  } else {
    if (init_val) {
      ss << (const_ ? " ::= " : " := ") << init_val->to_string(n);
    }
  }

  return ss.str();
}

// Note: This case covers MatchDeclaration too!
void Declaration::DependentDecls(DeclDepGraph *g, Declaration *d) const {
  g->graph_.add_edge(d, const_cast<Declaration *>(this));
  if (type_expr) {
    type_expr->DependentDecls(g, const_cast<Declaration *>(this));
  }
  if (init_val) {
    init_val->DependentDecls(g, const_cast<Declaration *>(this));
  }
}

ir::Results Declaration::EmitIr(Context *ctx) {
  bool swap_bc    = ctx->mod_ != mod_;
  Module *old_mod = std::exchange(ctx->mod_, mod_);
  if (swap_bc) { ctx->constants_ = &ctx->mod_->dep_data_.front(); }
  base::defer d([&] {
    ctx->mod_ = old_mod;
    if (swap_bc) { ctx->constants_ = &ctx->mod_->dep_data_.front(); }
  });

  if (const_) {
    // TODO
    if (is_fn_param_) {
      if (auto result = ctx->current_constants_.get_constant(this);
          !result.empty()) {
        return result;
      } else if (auto result = ctx->constants_->first.get_constant(this);
                 !result.empty()) {
        return result;
      } else {
        UNREACHABLE();
      }
    } else {
      auto *t = ASSERT_NOT_NULL(ctx->type_of(this));
      auto slot = ctx->constants_->second.constants_.reserve_slot(this, t);
      if (auto *result = std::get_if<ir::Results>(&slot)) {
        return std::move(*result);
      }

      auto &[data_offset, num_bytes] =
          std::get<std::pair<size_t, core::Bytes>>(slot);

      if (IsCustomInitialized()) {
        // TODO there's a lot of inefficiency here. `buf` is copied into the
        // constants slot and the copied to an ir::Results object to be
        // returned. In reality, we could write directly to the buffer and only
        // copy once if Evaluate* took an out-parameter.
        base::untyped_buffer buf = backend::EvaluateToBuffer(
            type::Typed<Expression const *>(init_val.get(), t), ctx);
        if (ctx->num_errors() > 0u) { return ir::Results{}; }
        return ctx->constants_->second.constants_.set_slot(
            data_offset, buf.raw(0), num_bytes);
      } else if (IsDefaultInitialized()) {
        //     if (is_fn_param_) {
        //       return ir::Results::FromVals(
        //           {ctx->mod_->constants_[ctx->bound_constants_].constants_.at(
        //               this)});
        //     } else {
        //       NOT_YET(this);
        //     }
        UNREACHABLE();
      } else {
        UNREACHABLE();
      }
    }
    UNREACHABLE(to_string(0));
  } else {
    // For local variables the declaration determines where the initial value is
    // set, but the allocation has to be done much earlier. We do the allocation
    // in FunctionLiteral::EmitIr. Declaration::EmitIr is just used to set the
    // value.
    ASSERT(scope_->Containing<core::FnScope>() != nullptr);

    // TODO these checks actually overlap and could be simplified.
    if (IsUninitialized()) { return ir::Results{}; }
    auto *t   = ctx->type_of(this);
    auto addr = ctx->addr(this);
    if (IsCustomInitialized()) {
      init_val->EmitMoveInit(type::Typed(addr, type::Ptr(t)), ctx);
    } else {
      if (!is_fn_param_) { t->EmitInit(addr, ctx); }
    }
    return ir::Results{addr};
  }
}

}  // namespace ast
