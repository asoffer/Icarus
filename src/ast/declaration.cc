#include "ast/declaration.h"

#include <sstream>
#include "ast/function_literal.h"
#include "ast/hole.h"
#include "ast/verify_macros.h"
#include "backend/eval.h"
#include "ir/func.h"
#include "module.h"
#include "type/all.h"

namespace AST {
namespace {
bool IsUninitialized(Declaration *decl) {
  return decl->init_val && decl->init_val->is<Hole>();
}

struct ArgumentMetaData {
  const type::Type *type;
  std::string name;
  bool has_default;
};

// TODO return a reason why this is not inferrable for better error messages.
bool Inferrable(const type::Type *t) {
  if (t == type::NullPtr || t == type::EmptyArray) {
    return false;
  } else if (t->is<type::Array>()) {
    return Inferrable(t->as<type::Array>().data_type);
  } else if (t->is<type::Pointer>()) {
    return Inferrable(t->as<type::Pointer>().pointee);
  } else if (t->is<type::Function>()) {
    const auto &f = t->as<type::Function>();
    for (auto *t : f.input) {
      if (!Inferrable(t)) { return false; }
    }
    for (auto *t : f.output) {
      if (!Inferrable(t)) { return false; }
    }
  }
  // TODO higher order types?
  return true;
}
// TODO: This algorithm is sufficiently complicated you should combine it
// with proof of correctness and good explanation of what it does.
bool CommonAmbiguousFunctionCall(const base::vector<ArgumentMetaData> &data1,
                                 const base::vector<ArgumentMetaData> &data2) {
  // TODO Don't need to reprocess this each time
  base::unordered_map<std::string, size_t> index2;
  for (size_t i = 0; i < data2.size(); ++i) { index2[data2[i].name] = i; }

  base::vector<int> delta_fwd_matches(std::max(data1.size(), data2.size()), 0);
  for (size_t i = 0; i < data1.size(); ++i) {
    auto iter = index2.find(data1[i].name);
    if (iter == index2.end()) { continue; }
    size_t j = iter->second;
    delta_fwd_matches[std::min(i, j)]++;
    delta_fwd_matches[std::max(i, j)]--;
  }

  base::vector<size_t> indices = {0};
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

bool Shadow(Declaration *decl1, Declaration *decl2, Context *ctx) {
  // TODO Don't worry about generic shadowing? It'll be checked later?
  if (decl1->type->is<type::Function>() || decl1->type == type::Generic ||
      decl2->type->is<type::Function>() || decl2->type == type::Generic) {
    return false;
  }

  // If they're both functions, we have more work to do because we allow
  // overloading so long as there are no ambiguous calls.

  // TODO can we store the data in this format to begin with?
  // TODO I don't need to fully generate code here, just the heading
  // information.
  // TODO check const-decl or not.

  auto ExtractMetaData = [](auto &eval) -> base::vector<ArgumentMetaData> {
    using eval_t = std::decay_t<decltype(eval)>;
    base::vector<ArgumentMetaData> metadata;

    if constexpr (std::is_same_v<eval_t, IR::Func *>) {
      metadata.reserve(eval->args_.size());
      for (size_t i = 0; i < eval->args_.size(); ++i) {
        metadata.push_back(ArgumentMetaData{
            /*        type = */ eval->type_->input[i],
            /*        name = */ eval->args_[i].first,
            /* has_default = */ eval->args_[i].second != nullptr});
      }
      return metadata;
    } else if constexpr (std::is_same_v<eval_t, Function *> ||
                         std::is_same_v<eval_t, GeneratedFunction *>) {
      metadata.reserve(eval->inputs.size());
      for (size_t i = 0; i < eval->inputs.size(); ++i) {
        metadata.push_back(ArgumentMetaData{
            /*        type = */ eval->inputs[i]->type,
            /*        name = */ eval->inputs[i]->identifier->token,
            /* has_default = */ !eval->inputs[i]->IsDefaultInitialized()});
      }
      // TODO Note the trickiness in names above. has_default if it isn't
      // default initailized. This is because IsDefaultInitialized means for
      // declarations that you do not have an "= something" part. It's just
      // the "foo: bar" part. But for function arguments, we call the "=
      // something" part the default.
      return metadata;
    } else {
      UNREACHABLE();
    }
  };

  auto val1 = backend::Evaluate(decl1->init_val.get(), ctx)[0];
  if (val1.type == nullptr) { return false; }
  auto metadata1 = std::visit(ExtractMetaData, val1.value);

  auto val2 = backend::Evaluate(decl2->init_val.get(), ctx)[0];
  if (val2.type == nullptr) { return false; }
  auto metadata2 = std::visit(ExtractMetaData, val2.value);

  return CommonAmbiguousFunctionCall(metadata1, metadata2);
}

}  // namespace

std::string Declaration::to_string(size_t n) const {
  std::stringstream ss;
  ss << identifier->to_string(n);
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
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  ASSERT(scope != nullptr);
  scope_ = scope;
  scope_->InsertDecl(this);
  identifier->assign_scope(scope);
  if (type_expr) { type_expr->assign_scope(scope); }
  if (init_val) { init_val->assign_scope(scope); }
}

bool Declaration::IsCustomInitialized() const {
  return init_val && !init_val->is<Hole>();
}

type::Type const *Declaration::VerifyType(Context *ctx) {
  type::Type const *this_type = nullptr;
  {
    VERIFY_STARTING_CHECK_EXPR;

    identifier->decl = this;

    type::Type const *type_expr_type = nullptr;
    type::Type const *init_val_type  = nullptr;
    if (type_expr) {
      type_expr_type = type_expr->VerifyType(ctx);
      HANDLE_CYCLIC_DEPENDENCIES;
      limit_to(type_expr);

      if (type_expr_type == type::Type_) {
        this_type =
            backend::EvaluateAs<type::Type const *>(type_expr.get(), ctx);
        ctx->mod_->types_.emplace(this, this_type);
      } else if (type_expr_type == type::Interface) {
        this_type = type::Generic;
        ctx->mod_->types_.emplace(this, type::Generic);
      } else {
        ctx->error_log_.NotAType(type_expr.get());
        limit_to(StageRange::Nothing());
        identifier->limit_to(StageRange::Nothing());
      }
    }

    if (this->IsCustomInitialized()) {
      init_val_type = init_val->VerifyType(ctx);
      HANDLE_CYCLIC_DEPENDENCIES;
      limit_to(init_val);

      if (init_val_type != nullptr) {
        if (!Inferrable(init_val_type)) {
          ctx->error_log_.UninferrableType(init_val->span);
          limit_to(StageRange::Nothing());
          identifier->limit_to(StageRange::Nothing());

        } else if (!type_expr) {
          this_type = init_val_type;
          ctx->mod_->types_.emplace(this, init_val_type);
        }
      }
    }

    if (type_expr && type_expr_type == type::Type_ && init_val &&
        !init_val->is<Hole>()) {
      if (!type::CanCastImplicitly(init_val_type, this_type)) {
        ctx->error_log_.AssignmentTypeMismatch(identifier.get(),
                                               init_val.get());
        limit_to(StageRange::Nothing());
      }
    }

    if (!type_expr) {
      ASSERT(init_val.get() != nullptr);
      if (!init_val->is<Hole>()) {  // I := V
        if (init_val_type == nullptr) {
          this_type = nullptr;
          limit_to(StageRange::Nothing());
          identifier->limit_to(StageRange::Nothing());
        }

      } else {  // I := --
        ctx->error_log_.InferringHole(span);
        this_type = init_val_type = nullptr;
        limit_to(StageRange::Nothing());
        init_val->limit_to(StageRange::Nothing());
        identifier->limit_to(StageRange::Nothing());
      }
    }

    if (const_ && init_val) {
      if (init_val->is<Hole>()) {
        ctx->error_log_.UninitializedConstant(span);
        limit_to(StageRange::Nothing());
        return nullptr;
      }
    }

    if (this_type == nullptr) {
      limit_to(StageRange::Nothing());
      return nullptr;
    }

    if (identifier->is<Hole>()) {
      if (this_type == type::Module) {
        // TODO check shadowing against other modules?
        // TODO what if no init val is provded? what if not constant?
        ctx->mod_->embedded_modules_.insert(
            backend::EvaluateAs<const Module *>(init_val.get(), ctx));
      } else {
        NOT_YET(this_type);
      }
    }
  }
  identifier->VerifyType(ctx);
  base::vector<TypedDecl> decls_to_check;
  {
    auto[good_decls_to_check, error_decls_to_check] =
        scope_->AllDeclsWithId(identifier->token, ctx);
    size_t num_total = good_decls_to_check.size() + error_decls_to_check.size();
    auto iter        = scope_->child_decls_.find(identifier->token);

    bool has_children = (iter != scope_->child_decls_.end());
    if (has_children) { num_total += iter->second.size(); }

    decls_to_check.reserve(num_total);
    decls_to_check.insert(decls_to_check.end(), good_decls_to_check.begin(),
                          good_decls_to_check.end());
    decls_to_check.insert(decls_to_check.end(), error_decls_to_check.begin(),
                          error_decls_to_check.end());

    if (has_children) {
      for (auto *decl : iter->second) {
        decls_to_check.emplace_back(ctx->mod_->types_.at(decl), decl);
      }
    }
  }

  auto iter =
      std::partition(decls_to_check.begin(), decls_to_check.end(),
                     [this](TypedDecl const &td) { return this >= td.decl_; });
  bool failed_shadowing = false;
  while (iter != decls_to_check.end()) {
    auto typed_decl = *iter;
    HANDLE_CYCLIC_DEPENDENCIES;
    if (Shadow(this, typed_decl.decl_, ctx)) {
      failed_shadowing = true;
      ctx->error_log_.ShadowingDeclaration(*this, *typed_decl.decl_);
      limit_to(StageRange::NoEmitIR());
    }
    ++iter;
  }

  if (failed_shadowing) {
    // TODO This may actually overshoot what we want. It may declare the
    // higher-up-the-scope-tree identifier as the shadow when something else on
    // a different branch could find it unambiguously. It's also just a hack
    // from the get-go so maybe we should just do it the right way.
    scope_->shadowed_decls_.insert(identifier->token);
    limit_to(StageRange::Nothing());
    return nullptr;
  }
  return this_type;
}

void Declaration::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  if (type_expr) { type_expr->Validate(ctx); }
  if (init_val) { init_val->Validate(ctx); }
}

void Declaration::SaveReferences(Scope *scope, base::vector<IR::Val> *args) {
  identifier->SaveReferences(scope, args);
  if (type_expr) { type_expr->SaveReferences(scope, args); }
  if (init_val) { init_val->SaveReferences(scope, args); }
}

void Declaration::contextualize(
    const Node *correspondant,
    const base::unordered_map<const Expression *, IR::Val> &replacements) {
  identifier->contextualize(correspondant->as<Declaration>().identifier.get(),
                            replacements);

  if (type_expr) {
    type_expr->contextualize(correspondant->as<Declaration>().type_expr.get(),
                             replacements);
  }
  if (init_val) {
    init_val->contextualize(correspondant->as<Declaration>().init_val.get(),
                            replacements);
  }
}

void Declaration::ExtractReturns(base::vector<const Expression *> *rets) const {
  identifier->ExtractReturns(rets);
  if (type_expr) { type_expr->ExtractReturns(rets); }
  if (init_val) { init_val->ExtractReturns(rets); }
}

Declaration *Declaration::Clone() const {
  auto *result = new Declaration;
  CloneTo(result);
  return result;
}

void Declaration::CloneTo(Declaration *result) const {
  result->span       = span;
  result->const_     = const_;
  result->identifier = base::wrap_unique(identifier->Clone());
  result->type_expr =
      type_expr ? base::wrap_unique(type_expr->Clone()) : nullptr;
  result->init_val = init_val ? base::wrap_unique(init_val->Clone()) : nullptr;
}

base::vector<IR::Val> AST::Declaration::EmitIR(Context *ctx) {
  if (const_) {
    // TODO it's custom or default initialized. cannot be uninitialized. This
    // should be verified by the type system.
    // TODO arg_val?
    auto[iter, newly_inserted] =
        ctx->mod_->bound_constants_.constants_.emplace(this, IR::Val::None());

    if (!newly_inserted) { return {iter->second}; }

    if (IsCustomInitialized()) {
      iter->second = backend::Evaluate(
          init_val.get(), ctx->mod_->types_.at(init_val.get()), ctx)[0];
      if (ctx->num_errors()) { return {}; }
      return {iter->second};
    } else if (IsDefaultInitialized()) {
      if (arg_val) {
        return {ctx->mod_->bound_constants_.constants_.at(this)};
      } else {
        NOT_YET(this);
      }
    } else {
      UNREACHABLE();
    }
  } else if (scope_ == ctx->mod_->global_.get()) {
    NOT_YET(to_string(0));
  } else {
    // For local variables the declaration determines where the initial value is
    // set, but the allocation has to be done much earlier. We do the allocation
    // in GenreatedFunction::EmitIR. Declaration::EmitIR is just used to set the
    // value.
    ASSERT(scope_->ContainingFnScope() != nullptr);

    // TODO these checks actually overlap and could be simplified.
    if (IsUninitialized(this)) { return {}; }
    auto *t = ctx->mod_->types_.at(this);
    if (IsCustomInitialized()) {
      type::EmitCopyInit(ctx->mod_->types_.at(init_val.get()), t,
                         init_val->EmitIR(ctx)[0], addr_, ctx);
    } else {
      t->EmitInit(addr_, ctx);
    }
    return {IR::Val::Reg(addr_, t)};
  }
}
}  // namespace AST
