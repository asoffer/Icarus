#include "ast/declaration.h"

#include <sstream>
#include "ast/function_literal.h"
#include "ast/hole.h"
#include "ast/verify_macros.h"
#include "backend/eval.h"
#include "ir/func.h"
#include "module.h"
#include "type/all.h"
#include "type/typed_value.h"

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

  auto ExtractMetaData = [ctx](auto &eval) -> base::vector<ArgumentMetaData> {
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
                         std::is_same_v<eval_t, FunctionLiteral *>) {
      metadata.reserve(eval->inputs.size());
      for (size_t i = 0; i < eval->inputs.size(); ++i) {
        auto *input_type = ctx->type_of(eval->inputs[i].get());
        metadata.push_back(ArgumentMetaData{
            /*        type = */ input_type,
            /*        name = */ eval->inputs[i]->id_,
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

bool Declaration::IsCustomInitialized() const {
  return init_val && !init_val->is<Hole>();
}

type::Type const *Declaration::VerifyType(Context *ctx) {
  Module *old_mod = std::exchange(ctx->mod_, mod_);
  base::defer d([&] { ctx->mod_ = old_mod; });

  type::Type const *this_type = nullptr;
  {
    type::Type const *type_expr_type = nullptr;
    type::Type const *init_val_type  = nullptr;
    if (type_expr) {
      type_expr_type = type_expr->VerifyType(ctx);
      HANDLE_CYCLIC_DEPENDENCIES;
      limit_to(type_expr);

      if (type_expr_type == type::Type_) {
        this_type =
            backend::EvaluateAs<type::Type const *>(type_expr.get(), ctx);
        ctx->set_type(this, this_type);
      } else if (type_expr_type == type::Interface) {
        this_type = type::Generic;
        ctx->set_type(this, type::Generic);
      } else {
        ctx->error_log_.NotAType(type_expr.get());
        limit_to(StageRange::Nothing());
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

        } else if (!type_expr) {
          this_type = init_val_type;
          ctx->set_type(this, init_val_type);
        }
      }
    }

    if (type_expr && type_expr_type == type::Type_ && init_val &&
        !init_val->is<Hole>()) {
      if (!type::CanCastImplicitly(init_val_type, this_type)) {
        ctx->error_log_.AssignmentTypeMismatch(this, init_val.get());
        limit_to(StageRange::Nothing());
      }
    }

    if (!type_expr) {
      ASSERT(init_val.get() != nullptr);
      if (!init_val->is<Hole>()) {  // I := V
        if (init_val_type == nullptr) {
          this_type = nullptr;
          limit_to(StageRange::Nothing());
        }

      } else {  // I := --
        ctx->error_log_.InferringHole(span);
        this_type = init_val_type = nullptr;
        limit_to(StageRange::Nothing());
        init_val->limit_to(StageRange::Nothing());
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

    if (id_.empty()) {
      if (this_type == type::Module) {
        // TODO check shadowing against other modules?
        // TODO what if no init val is provded? what if not constant?
        ctx->mod_->embedded_modules_.insert(
            backend::EvaluateAs<const Module *>(init_val.get(), ctx));
        return type::Module;
      } else {
        NOT_YET(this_type);
      }
    }
  }

  base::vector<type::Typed<Declaration *>> decls_to_check;
  {
    auto[good_decls_to_check, error_decls_to_check] =
        scope_->AllDeclsWithId(id_, ctx);
    size_t num_total = good_decls_to_check.size() + error_decls_to_check.size();
    auto iter        = scope_->child_decls_.find(id_);

    bool has_children = (iter != scope_->child_decls_.end());
    if (has_children) { num_total += iter->second.size(); }

    decls_to_check.reserve(num_total);
    decls_to_check.insert(decls_to_check.end(), good_decls_to_check.begin(),
                          good_decls_to_check.end());
    decls_to_check.insert(decls_to_check.end(), error_decls_to_check.begin(),
                          error_decls_to_check.end());

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
    HANDLE_CYCLIC_DEPENDENCIES;
    if (Shadow(this, typed_decl.get(), ctx)) {
      failed_shadowing = true;
      ctx->error_log_.ShadowingDeclaration(*this, *typed_decl);
      limit_to(StageRange::NoEmitIR());
    }
    ++iter;
  }

  if (failed_shadowing) {
    // TODO This may actually overshoot what we want. It may declare the
    // higher-up-the-scope-tree identifier as the shadow when something else on
    // a different branch could find it unambiguously. It's also just a hack
    // from the get-go so maybe we should just do it the right way.
    scope_->shadowed_decls_.insert(id_);
    limit_to(StageRange::Nothing());
    return nullptr;
  }
  return this_type;
}

void Declaration::Validate(Context *ctx) {
  Module *old_mod = std::exchange(ctx->mod_, mod_);
  base::defer d([&] { ctx->mod_ = old_mod; });
  if (type_expr) { type_expr->Validate(ctx); }
  if (init_val) { init_val->Validate(ctx); }
}

void Declaration::ExtractJumps(JumpExprs *rets) const {
  if (type_expr) { type_expr->ExtractJumps(rets); }
  if (init_val) { init_val->ExtractJumps(rets); }
}

base::vector<IR::Val> AST::Declaration::EmitIR(Context *ctx) {
  Module *old_mod = std::exchange(ctx->mod_, mod_);
  base::defer d([&] { ctx->mod_ = old_mod; });

  if (const_) {
    if (is_arg_) {
      return {ctx->bound_constants_.constants_.at(this)};
    } else {
      auto[iter, newly_inserted] =
          ctx->mod_->constants_[ctx->bound_constants_].constants_.emplace(
              this, IR::Val::None());
      if (!newly_inserted) { return {iter->second}; }

      if (IsCustomInitialized()) {
        iter->second = backend::Evaluate(init_val.get(), ctx)[0];
        if (ctx->num_errors()) { return {}; }
        return {iter->second};
      } else if (IsDefaultInitialized()) {
        if (is_arg_) {
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
      type::EmitCopyInit(ctx->type_of(init_val.get()), t,
                         init_val->EmitIR(ctx)[0], addr, ctx);
    } else {
      t->EmitInit(addr, ctx);
    }
    return {IR::Val::Reg(addr, t)};
  }
}
}  // namespace AST
