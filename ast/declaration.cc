#include "ast/declaration.h"

#include <sstream>
#include "ast/function_literal.h"
#include "backend/eval.h"
#include "error/inference_failure_reason.h"
#include "ir/func.h"
#include "misc/module.h"
#include "type/cast.h"
#include "type/typed_value.h"

namespace ast {
namespace {

InferenceFailureReason Inferrable(type::Type const *t) {
  if (t == type::NullPtr) {
    return InferenceFailureReason::NullPtr;
  } else if (t == type::EmptyArray) {
    return InferenceFailureReason::EmptyArray;
  } else if (t->is<type::Array>()) {
    return Inferrable(t->as<type::Array>().data_type);
  } else if (t->is<type::Pointer>()) {
    return Inferrable(t->as<type::Pointer>().pointee);
  } else if (auto *v = t->if_as<type::Variant>()) {
    // TODO only returning the first failure here and not even givving a good
    // explanation of precisely what the problem is. Fix here and below.
    for (auto const *var : v->variants_) {
      auto reason = Inferrable(var);
      if (reason != InferenceFailureReason::Inferrable) { return reason; }
    }
  } else if (auto *tup = t->if_as<type::Tuple>()) {
    for (auto const *entry : tup->entries_) {
      auto reason = Inferrable(entry);
      if (reason != InferenceFailureReason::Inferrable) { return reason; }
    }
  } else if (auto *f = t->if_as<type::Function>()) {
    for (auto const *t : f->input) {
      auto reason = Inferrable(t);
      if (reason != InferenceFailureReason::Inferrable) { return reason; }
    }
    for (auto const *t : f->output) {
      auto reason = Inferrable(t);
      if (reason != InferenceFailureReason::Inferrable) { return reason; }
    }
  }
  // TODO higher order types?
  return InferenceFailureReason::Inferrable;
}

// TODO what about shadowing of symbols across module boundaries imported with
// -- ::= ?
// Or when you import two modules verifying that symbols don't conflict.
bool Shadow(type::Typed<Declaration *> decl1, type::Typed<Declaration *> decl2,
            Context *ctx) {
  // TODO Don't worry about generic shadowing? It'll be checked later?
  if (decl1.type() == type::Generic || decl2.type() == type::Generic) {
    return false;
  }

  auto ExtractParams = +[](bool is_const, Expression *expr,
                           Context *ctx) -> core::FnParams<type::Type const *> {
    if (!is_const) {
      return ctx->type_of(expr)
          ->as<type::Function>()
          .AnonymousFnParams()
          .Transform(
              [](type::Typed<Expression *> expr) { return expr.type(); });
    } else if (auto *fn_lit = expr->if_as<FunctionLiteral>()) {
      return fn_lit->inputs_.Transform(
          [ctx](std::unique_ptr<Declaration> const &decl) {
            return ctx->type_of(decl.get());
          });
    }
    NOT_YET();
  };
  return core::AmbiguouslyCallable(
      ExtractParams(decl1.get()->const_, (*decl1)->init_val.get(), ctx),
      ExtractParams(decl2.get()->const_, (*decl2)->init_val.get(), ctx),
      [](type::Type const *lhs, type::Type const *rhs) {
        return type::Meet(lhs, rhs) != nullptr;
      });
}

enum DeclKind { INFER = 1, CUSTOM_INIT = 2, UNINITIALIZED = 4 };
}  // namespace

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

void Declaration::assign_scope(core::Scope *scope) {
  ASSERT(scope != nullptr);
  scope_ = scope;
  scope_->InsertDecl(this);
  if (type_expr) { type_expr->assign_scope(scope); }
  if (init_val) { init_val->assign_scope(scope); }
}

// Note: This case covers MatchDeclaration too!
void Declaration::DependentDecls(base::Graph<Declaration *> *g,
                                 Declaration *d) const {
  g->add_edge(d, const_cast<Declaration *>(this));
  if (type_expr) {
    type_expr->DependentDecls(g, const_cast<Declaration *>(this));
  }
  if (init_val) {
    init_val->DependentDecls(g, const_cast<Declaration *>(this));
  }
}

VerifyResult VerifySpecialFunctions(Declaration const *decl,
                                    type::Type const *decl_type, Context *ctx) {
  bool error = false;
  if (decl->id_ == "copy") {
    if (auto *f = decl_type->if_as<type::Function>()) {
      if (!f->output.empty()) {
        error = true;
        NOT_YET("output must be empty");
      }

      if (f->input.size() != 2 || f->input.at(0) != f->input.at(1) ||
          !f->input.at(0)->is<type::Pointer>() ||
          !f->input.at(0)->as<type::Pointer>().pointee->is<type::Struct>()) {
        error = true;
        NOT_YET("incorrect input type");
      } else {
        // TODO should you check that they're exported consistently in some way?
        // Note that you don't export the struct but rather declarations bound
        // to it so it's not totally clear how you would do that.
        auto const &s =
            f->input.at(0)->as<type::Pointer>().pointee->as<type::Struct>();

        if (decl->scope_ != s.scope_) {
          error = true;
          NOT_YET(
              "(copy) must be defined in the same scope as the corresponding "
              "type");
        }

        if (s.contains_hashtag(Hashtag::Builtin::Uncopyable)) {
          NOT_YET("defined (copy) on a non-copyable type");
        }
      }
    } else {
      error = true;
      NOT_YET("log an error. (copy) must be a function.");
    }
  } else if (decl->id_ == "move") {
    if (auto *f = decl_type->if_as<type::Function>()) {
      if (!f->output.empty()) {
        error = true;
        NOT_YET("output must be empty");
      }

      if (f->input.size() != 2 || f->input.at(0) != f->input.at(1) ||
          !f->input.at(0)->is<type::Pointer>() ||
          !f->input.at(0)->as<type::Pointer>().pointee->is<type::Struct>()) {
        error = true;
        NOT_YET("incorrect input type");
      } else {
        // TODO should you check that they're exported consistently in some way?
        // Note that you don't export the struct but rather declarations bound
        // to it so it's not totally clear how you would do that.
        auto const &s =
            f->input.at(0)->as<type::Pointer>().pointee->as<type::Struct>();

        if (decl->scope_ != s.scope_) {
          error = true;
          NOT_YET(
              "(move) must be defined in the same scope as the corresponding "
              "type");
        }

        if (s.contains_hashtag(Hashtag::Builtin::Immovable)) {
          error = true;
          NOT_YET("defined (move) for an immovable type");
        }
      }
    } else {
      error = true;
      NOT_YET("log an error. (move) must be a function.");
    }
  }
  if (error) { ctx->set_result(decl, VerifyResult::Error()); }

  return ctx->set_result(decl, VerifyResult(decl_type, decl->const_));
}

VerifyResult Declaration::VerifyType(Context *ctx) {
  bool swap_bc    = ctx->mod_ != mod_;
  Module *old_mod = std::exchange(ctx->mod_, mod_);
  BoundConstants old_bc;
  if (swap_bc) {
    old_bc = std::exchange(ctx->bound_constants_, BoundConstants{});
  }
  base::defer d([&] {
    ctx->mod_ = old_mod;
    if (swap_bc) { ctx->bound_constants_ = std::move(old_bc); }
  });

  // Declarations may have already been computed. Essentially the first time we
  // see an identifier (either a real identifier node, or a declaration, we need
  // to verify the type, but we only want to do this once.
  if (auto *attempt = ctx->prior_verification_attempt(this)) {
    return *attempt;
  }

  int dk = 0;
  if (IsInferred()) { dk = INFER; }
  if (IsUninitialized()) {
    dk |= UNINITIALIZED;
  } else if (IsCustomInitialized()) {
    dk |= CUSTOM_INIT;
  }

  type::Type const *this_type = nullptr;
  switch (dk) {
    case 0 /* Default initailization */: {
      ASSIGN_OR(return ctx->set_result(this, VerifyResult::Error()),
                       auto type_expr_result, type_expr->VerifyType(ctx));
      if (!type_expr_result.const_) {
        // Hmm, not necessarily an error. Example (not necessarily minimal):
        //
        //   S ::= (x: any`T) => struct {
        //     _val: T = x
        //   }
        //
        NOT_YET("log an error", this);
        return ctx->set_result(this, VerifyResult::Error());
      }
      auto *type_expr_type = type_expr_result.type_;
      if (type_expr_type == type::Type_) {
        this_type = ASSERT_NOT_NULL(
            ctx->set_result(this, VerifyResult::Constant(
                                      backend::EvaluateAs<type::Type const *>(
                                          type_expr.get(), ctx)))
                .type_);

        if (!is_fn_param_ && !this_type->IsDefaultInitializable()) {
          ctx->error_log()->TypeMustBeInitialized(span, this_type);
        }

      } else if (type_expr_type == type::Intf) {
        if (!type_expr_result.const_) {
          NOT_YET("log an error");
          return ctx->set_result(this, VerifyResult::Error());
        } else {
          this_type =
              ctx->set_result(this, VerifyResult::Constant(
                                        backend::EvaluateAs<type::Type const *>(
                                            type_expr.get(), ctx)))
                  .type_;
        }
      } else {
        ctx->error_log()->NotAType(type_expr->span, type_expr_type);
        return ctx->set_result(this, VerifyResult::Error());
      }
    } break;
    case INFER: UNREACHABLE(); break;
    case INFER | CUSTOM_INIT: {
      ASSIGN_OR(return ctx->set_result(this, VerifyResult::Error()),
                       auto init_val_result, init_val->VerifyType(ctx));
      auto reason = Inferrable(init_val_result.type_);
      if (reason != InferenceFailureReason::Inferrable) {
        ctx->error_log()->UninferrableType(reason, init_val->span);
        return ctx->set_result(this, VerifyResult::Error());
      }

      // TODO initialization, not assignment.
      if (!type::VerifyAssignment(span, init_val_result.type_,
                                  init_val_result.type_, ctx)) {
        return ctx->set_result(this, VerifyResult::Error());
      }

      this_type =
          ctx->set_result(this, VerifyResult(init_val_result.type_, const_))
              .type_;

    } break;
    case INFER | UNINITIALIZED: {
      ctx->error_log()->UninferrableType(InferenceFailureReason::Hole,
                                         init_val->span);
      if (const_) { ctx->error_log()->UninitializedConstant(span); }
      return ctx->set_result(this, VerifyResult::Error());
    } break;
    case CUSTOM_INIT: {
      auto init_val_result = init_val->VerifyType(ctx);
      bool error           = !init_val_result.ok();

      auto *init_val_type   = init_val->VerifyType(ctx).type_;
      auto type_expr_result = type_expr->VerifyType(ctx);
      auto *type_expr_type  = type_expr_result.type_;

      if (type_expr_type == nullptr) {
        error = true;
      } else if (type_expr_type == type::Type_) {
        if (!type_expr_result.const_) {
          NOT_YET("log an error");
          error = true;
        } else {
          this_type =
              ctx->set_result(this, VerifyResult::Constant(
                                        backend::EvaluateAs<type::Type const *>(
                                            type_expr.get(), ctx)))
                  .type_;
        }

        // TODO initialization, not assignment. Error messages will be
        // wrong.
        if (this_type != nullptr && init_val_type != nullptr) {
          error |= !type::VerifyAssignment(span, this_type, init_val_type, ctx);
        }
      } else if (type_expr_type == type::Intf) {
        this_type =
            ctx->set_result(this, VerifyResult::Constant(type::Generic)).type_;
      } else {
        ctx->error_log()->NotAType(type_expr->span, type_expr_type);
        error = true;
      }

      if (error) { return ctx->set_result(this, VerifyResult::Error()); }
    } break;
    case UNINITIALIZED: {
      ASSIGN_OR(return ctx->set_result(this, VerifyResult::Error()),
                       auto type_expr_result, type_expr->VerifyType(ctx));
      auto *type_expr_type = type_expr_result.type_;
      if (type_expr_type == type::Type_) {
        if (!type_expr_result.const_) {
          NOT_YET("log an error");
          return ctx->set_result(this, VerifyResult::Error());
        }
        this_type =
            ctx->set_result(this, VerifyResult::Constant(
                                      backend::EvaluateAs<type::Type const *>(
                                          type_expr.get(), ctx)))
                .type_;
      } else if (type_expr_type == type::Intf) {
        this_type =
            ctx->set_result(this, VerifyResult::Constant(type::Generic)).type_;
      } else {
        ctx->error_log()->NotAType(type_expr->span, type_expr_type);
        return ctx->set_result(this, VerifyResult::Error());
      }

      if (const_) {
        ctx->error_log()->UninitializedConstant(span);
        return ctx->set_result(this, VerifyResult::Error());
      }

    } break;
    default: UNREACHABLE(dk);
  }

  if (id_.empty()) {
    if (this_type == type::Module) {
      // TODO check shadowing against other modules?
      // TODO what if no init val is provded? what if not constant?
      scope_->embedded_modules_.insert(
          backend::EvaluateAs<Module const *>(init_val.get(), ctx));
      return ctx->set_result(this, VerifyResult::Constant(type::Module));
    } else if (this_type->is<type::Tuple>()) {
      NOT_YET(this_type , to_string(0));
    } else {
      NOT_YET(this_type ,to_string(0));
    }
  }

  // TODO simplify now that you don't have error decls.
  ASSERT(this_type != nullptr) << to_string(0);
  std::vector<type::Typed<Declaration *>> decls_to_check;
  {
    auto good_decls_to_check = scope_->AllDeclsWithId(id_, ctx);
    size_t num_total         = good_decls_to_check.size();
    auto iter                = scope_->child_decls_.find(id_);

    bool has_children = (iter != scope_->child_decls_.end());
    if (has_children) { num_total += iter->second.size(); }

    decls_to_check.reserve(num_total);
    decls_to_check.insert(decls_to_check.end(), good_decls_to_check.begin(),
                          good_decls_to_check.end());

    if (has_children) {
      for (auto *decl : iter->second) {
        decls_to_check.emplace_back(decl, ctx->type_of(decl));
      }
    }
  }

  auto iter = std::partition(
      decls_to_check.begin(), decls_to_check.end(),
      [this](type::Typed<Declaration *> td) { return this >= td.get(); });
  bool failed_shadowing = false;
  while (iter != decls_to_check.end()) {
    auto typed_decl = *iter;
    if (Shadow(type::Typed(this, this_type), typed_decl, ctx)) {
      failed_shadowing = true;
      ctx->error_log()->ShadowingDeclaration(span, (*typed_decl)->span);
    }
    ++iter;
  }

  if (failed_shadowing) {
    // TODO This may actually overshoot what we want. It may declare the
    // higher-up-the-scope-tree identifier as the shadow when something else on
    // a different branch could find it unambiguously. It's also just a hack
    // from the get-go so maybe we should just do it the right way.
    return ctx->set_result(this, VerifyResult::Error());
  }

  return VerifySpecialFunctions(this, this_type, ctx);
}

void Declaration::ExtractJumps(JumpExprs *rets) const {
  if (type_expr) { type_expr->ExtractJumps(rets); }
  if (init_val) { init_val->ExtractJumps(rets); }
}

ir::Results Declaration::EmitIr(Context *ctx) {
  bool swap_bc    = ctx->mod_ != mod_;
  Module *old_mod = std::exchange(ctx->mod_, mod_);
  BoundConstants old_bc;
  if (swap_bc) {
    old_bc = std::exchange(ctx->bound_constants_, BoundConstants{});
  }
  base::defer d([&] {
    ctx->mod_ = old_mod;
    if (swap_bc) { ctx->bound_constants_ = std::move(old_bc); }
  });

  if (const_) {
    if (is_fn_param_) {
      return ir::Results::FromVals({ctx->bound_constants_.constants_.at(this)});
    } else {
      auto [iter, newly_inserted] =
          ctx->mod_->constants_[ctx->bound_constants_].constants_.emplace(
              this, ir::Val::None());
      if (!newly_inserted) { return ir::Results::FromVals({iter->second}); }

      if (IsCustomInitialized()) {
        auto vals    = backend::Evaluate(init_val.get(), ctx);
        iter->second = vals[0];
        if (ctx->num_errors() > 0u) { return ir::Results{}; }
        return ir::Results::FromVals({iter->second});
      } else if (IsDefaultInitialized()) {
        if (is_fn_param_) {
          return ir::Results::FromVals(
              {ctx->mod_->constants_[ctx->bound_constants_].constants_.at(
                  this)});
        } else {
          NOT_YET(this);
        }
      } else {
        UNREACHABLE();
      }
    }
  } else {
    // For local variables the declaration determines where the initial value is
    // set, but the allocation has to be done much earlier. We do the allocation
    // in FunctionLiteral::EmitIr. Declaration::EmitIr is just used to set the
    // value.
    ASSERT(scope_->ContainingFnScope() != nullptr);

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
