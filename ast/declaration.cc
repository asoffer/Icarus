#include "ast/declaration.h"

#include <sstream>
#include "ast/function_literal.h"
#include "ast/hole.h"
#include "backend/eval.h"
#include "error/inference_failure_reason.h"
#include "ir/func.h"
#include "misc/module.h"
#include "type/cast.h"
#include "type/typed_value.h"

namespace ast {
namespace {
bool IsUninitialized(Declaration *decl) {
  return decl->init_val && decl->init_val->is<Hole>();
}

struct ArgumentMetaData {
  const type::Type *type;
  std::string name;
  bool has_default;
};

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

// TODO: This algorithm is sufficiently complicated you should combine it
// with proof of correctness and good explanation of what it does.
bool CommonAmbiguousFunctionCall(const std::vector<ArgumentMetaData> &data1,
                                 const std::vector<ArgumentMetaData> &data2) {
  // TODO Don't need to reprocess this each time
  std::unordered_map<std::string, size_t> index2;
  for (size_t i = 0; i < data2.size(); ++i) { index2[data2[i].name] = i; }

  std::vector<int> delta_fwd_matches(std::max(data1.size(), data2.size()), 0);
  for (size_t i = 0; i < data1.size(); ++i) {
    auto iter = index2.find(data1[i].name);
    if (iter == index2.end()) { continue; }
    size_t j = iter->second;
    delta_fwd_matches[std::min(i, j)]++;
    delta_fwd_matches[std::max(i, j)]--;
  }

  std::vector<size_t> indices = {0};
  // One useful invariant here is that accumulating delta_fwd_matches always
  // yields a non-negative integer. This is because any subtraction that
  // occurs is always preceeded by an addition.
  size_t accumulator = 0;
  for (size_t i = 0; i < delta_fwd_matches.size(); ++i) {
    if (data1[i].type != data2[i].type) { break; }
    accumulator += delta_fwd_matches[i];
    if (accumulator == 0) { indices.push_back(i + 1); }
  }

  // TODO working backwards through indices should allow you to avoid having
  // to copy index2 each time and redo the same work repeatedly.
  for (auto index : indices) {
    // Everything after this index would have to be named or defaulted.
    // named values that match but with different types would have to be
    // defaulted.
    auto index2_copy = index2;
    for (size_t i = index; i < data1.size(); ++i) {
      auto iter = index2_copy.find(data1[i].name);
      if (iter == index2_copy.end()) {
        if (!data1[i].has_default) { goto next_option; }
      } else {
        size_t j = iter->second;
        // TODO not just equal but if there exists something convertible to
        // both.
        if (data1[i].type != data2[j].type &&
            (!data1[i].has_default || !data2[j].has_default)) {
          goto next_option;
        }

        // These two parameters can both be named explicitly. Remove it from
        // index2_copy so what we're left with are all those elements in the
        // second function which haven't been named by anything in the
        // first.
        index2_copy.erase(iter);
      }
    }

    for (const auto &entry : index2) {
      if (entry.second < index) {
        // Ignore entries which preceed the index where we start using named
        // arguments.
        continue;
      }
      // Each of these must have a default if there's a call to both.
      if (!data2[entry.second].has_default) { goto next_option; }
    }

    return true;

  next_option:;
  }
  return false;
}

// TODO what about shadowing of symbols across module boundaries imported with
// -- ::= ?
// Or when you import two modules verifying that symbols don't conflict.
bool Shadow(Declaration *decl1, Declaration *decl2, Context *ctx) {
  auto *decl1_type = ctx->type_of(decl1);
  auto *decl2_type = ctx->type_of(decl2);
  // TODO Don't worry about generic shadowing? It'll be checked later?
  if (decl1_type->is<type::Function>() || decl1_type == type::Generic ||
      decl2_type->is<type::Function>() || decl2_type == type::Generic) {
    return false;
  }

  // If they're both functions, we have more work to do because we allow
  // overloading so long as there are no ambiguous calls.

  // TODO can we store the data in this format to begin with?
  // TODO I don't need to fully generate code here, just the heading
  // information.
  // TODO check const-decl or not.

  auto ExtractMetaData = [ctx](auto &eval) -> std::vector<ArgumentMetaData> {
    using eval_t = std::decay_t<decltype(eval)>;
    std::vector<ArgumentMetaData> metadata;

    if constexpr (std::is_same_v<eval_t, ir::Func *>) {
      metadata.reserve(eval->args_.size());
      for (size_t i = 0; i < eval->args_.size(); ++i) {
        metadata.push_back(ArgumentMetaData{
            /*        type = */ eval->type_->input[i],
            /*        name = */ eval->params_.at(i).name,
            /* has_default = */ eval->args_.at(i).value != nullptr});
      }
      return metadata;
    } else if constexpr (std::is_same_v<eval_t, Function *> ||
                         std::is_same_v<eval_t, FunctionLiteral *>) {
      metadata.reserve(eval->inputs_.size());
      for (size_t i = 0; i < eval->inputs_.size(); ++i) {
        auto *input_type = ctx->type_of(eval->inputs_.at(i).value.get());
        metadata.push_back(ArgumentMetaData{
            /*        type = */ input_type,
            /*        name = */ eval->inputs_.at(i).value->id_,
            /* has_default = */
            !eval->inputs_.at(i).value->IsDefaultInitialized()});
      }
      // TODO Note the trickiness in names above. has_default if it isn't
      // default initailized. This is because IsDefaultInitialized means for
      // declarations that you do not have an "= something" part. It's just
      // the "foo: bar" part. But for function arguments, we call the "=
      // something" part the default.
      return metadata;
    } else {
      UNREACHABLE(typeid(eval_t).name());
    }
  };

  // TODO are val1 and val2 necessarily const? Do we actually need to evaluate
  // them?
  auto val1 = backend::Evaluate(decl1->init_val.get(), ctx)[0];
  if (val1.type == nullptr) { return false; }
  auto metadata1 = std::visit(ExtractMetaData, val1.value);

  auto val2 = backend::Evaluate(decl2->init_val.get(), ctx)[0];
  if (val2.type == nullptr) { return false; }
  auto metadata2 = std::visit(ExtractMetaData, val2.value);

  return CommonAmbiguousFunctionCall(metadata1, metadata2);
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

void Declaration::assign_scope(Scope *scope) {
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

bool Declaration::IsCustomInitialized() const {
  return init_val && !init_val->is<Hole>();
}

VerifyResult VerifySpecialFunctions(Declaration const *decl,
                                    type::Type const *decl_type) {
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
  if (error) { return VerifyResult::Error(); }

  return VerifyResult(decl_type, decl->const_);
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

  int dk = 0;
  if (type_expr == nullptr || type_expr->is<Hole>()) { dk = INFER; }
  if (init_val && init_val->is<Hole>()) {
    dk |= UNINITIALIZED;
  } else if (init_val) {
    dk |= CUSTOM_INIT;
  }

  type::Type const *this_type = nullptr;
  switch (dk) {
    case 0 /* Default initailization */: {
      ASSIGN_OR(return VerifyResult::Error(), auto type_expr_result,
                       type_expr->VerifyType(ctx));
      if (!type_expr_result.const_) {
        // Hmm, not necessarily an error. Example (not necessarily minimal):
        //
        //   S ::= (x: any`T) => struct {
        //     _val: T = x
        //   }
        //
        NOT_YET("log an error", this);
        return VerifyResult::Error();
      }
      auto *type_expr_type = type_expr_result.type_;
      if (type_expr_type == type::Type_) {
        this_type = ctx->set_type(
            this, ASSERT_NOT_NULL(backend::EvaluateAs<type::Type const *>(
                      type_expr.get(), ctx)));

        if (!is_fn_param_ && !this_type->IsDefaultInitializable()) {
          ctx->error_log_.TypeMustBeInitialized(span, this_type);
        }

      } else if (type_expr_type == type::Intf) {
        if (!type_expr_result.const_) {
          NOT_YET("log an error");
          return VerifyResult::Error();
        } else {
          this_type = ctx->set_type(
              this,
              backend::EvaluateAs<type::Type const *>(type_expr.get(), ctx));
        }
      } else {
        ctx->error_log_.NotAType(type_expr->span, type_expr_type);
        return VerifyResult::Error();
      }
    } break;
    case INFER: UNREACHABLE(); break;
    case INFER | CUSTOM_INIT: {
      ASSIGN_OR(return VerifyResult::Error(), auto init_val_result,
                       init_val->VerifyType(ctx));
      auto *init_val_type = init_val_result.type_;
      auto reason         = Inferrable(init_val_type);
      if (reason != InferenceFailureReason::Inferrable) {
        ctx->error_log_.UninferrableType(reason, init_val->span);
        return VerifyResult::Error();
      }

      this_type = ctx->set_type(this, init_val_type);

      // TODO initialization, not assignment.
      if (!type::VerifyAssignment(span, this_type, this_type, ctx)) {
        return VerifyResult::Error();
      }

    } break;
    case INFER | UNINITIALIZED: {
      ctx->error_log_.UninferrableType(InferenceFailureReason::Hole,
                                       init_val->span);
      if (const_) { ctx->error_log_.UninitializedConstant(span); }
      return VerifyResult::Error();
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
          this_type = ctx->set_type(
              this,
              backend::EvaluateAs<type::Type const *>(type_expr.get(), ctx));
        }

        // TODO initialization, not assignment. Error messages will be
        // wrong.
        if (this_type != nullptr && init_val_type != nullptr) {
          error |= !type::VerifyAssignment(span, this_type, init_val_type, ctx);
        }
      } else if (type_expr_type == type::Intf) {
        this_type = ctx->set_type(this, type::Generic);
      } else {
        ctx->error_log_.NotAType(type_expr->span, type_expr_type);
        error = true;
      }

      if (error) { return VerifyResult::Error(); }
    } break;
    case UNINITIALIZED: {
      ASSIGN_OR(return VerifyResult::Error(), auto type_expr_result,
                       type_expr->VerifyType(ctx));
      auto *type_expr_type = type_expr_result.type_;
      if (type_expr_type == type::Type_) {
        if (!type_expr_result.const_) {
          NOT_YET("log an error");
          return VerifyResult::Error();
        }
        this_type = ctx->set_type(this, backend::EvaluateAs<type::Type const *>(
                                            type_expr.get(), ctx));
      } else if (type_expr_type == type::Intf) {
        this_type = ctx->set_type(this, type::Generic);
      } else {
        ctx->error_log_.NotAType(type_expr->span, type_expr_type);
        return VerifyResult::Error();
      }

      if (const_) {
        ctx->error_log_.UninitializedConstant(span);
        return VerifyResult::Error();
      }

    } break;
    default:
      UNREACHABLE(dk);

      if (id_.empty()) {
        if (this_type == type::Module) {
          // TODO check shadowing against other modules?
          // TODO what if no init val is provded? what if not constant?
          scope_->embedded_modules_.insert(
              backend::EvaluateAs<Module const *>(init_val.get(), ctx));
          return VerifyResult::Constant(type::Module);
        } else if (this_type->is<type::Tuple>()) {
          NOT_YET(this_type);
        } else {
          NOT_YET(this_type);
        }
      }
  }

  // TODO simplify now that you don't have error decls.
  ASSERT(this_type != nullptr) << this;
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
    if (Shadow(this, typed_decl.get(), ctx)) {
      failed_shadowing = true;
      ctx->error_log_.ShadowingDeclaration(*this, *typed_decl);
    }
    ++iter;
  }

  if (failed_shadowing) {
    // TODO This may actually overshoot what we want. It may declare the
    // higher-up-the-scope-tree identifier as the shadow when something else on
    // a different branch could find it unambiguously. It's also just a hack
    // from the get-go so maybe we should just do it the right way.
    scope_->shadowed_decls_.insert(id_);
    return VerifyResult::Error();
  }

  return VerifySpecialFunctions(this, this_type);
}

void Declaration::Validate(Context *ctx) {
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

  if (type_expr) { type_expr->Validate(ctx); }
  if (init_val) { init_val->Validate(ctx); }
}

void Declaration::ExtractJumps(JumpExprs *rets) const {
  if (type_expr) { type_expr->ExtractJumps(rets); }
  if (init_val) { init_val->ExtractJumps(rets); }
}

std::vector<ir::Val> ast::Declaration::EmitIR(Context *ctx) {
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
      return {ctx->bound_constants_.constants_.at(this)};
    } else {
      auto [iter, newly_inserted] =
          ctx->mod_->constants_[ctx->bound_constants_].constants_.emplace(
              this, ir::Val::None());
      if (!newly_inserted) { return {iter->second}; }

      if (IsCustomInitialized()) {
        iter->second = backend::Evaluate(init_val.get(), ctx)[0];
        if (ctx->num_errors()) { return {}; }
        return {iter->second};
      } else if (IsDefaultInitialized()) {
        if (is_fn_param_) {
          return {
              ctx->mod_->constants_[ctx->bound_constants_].constants_.at(this)};
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
    // in FunctionLiteral::EmitIR. Declaration::EmitIR is just used to set the
    // value.
    ASSERT(scope_->ContainingFnScope() != nullptr);

    // TODO these checks actually overlap and could be simplified.
    if (IsUninitialized(this)) { return {}; }
    auto *t   = ctx->type_of(this);
    auto addr = ctx->addr(this);
    if (IsCustomInitialized()) {
      init_val->EmitMoveInit(type::Typed(addr, type::Ptr(t)), ctx);
    } else {
      if (!is_fn_param_) { t->EmitInit(addr, ctx); }
    }
    return {ir::Val::Reg(addr, t)};
  }
}

}  // namespace ast
