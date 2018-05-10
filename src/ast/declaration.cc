#include "ast/declaration.h"

#include <sstream>
#include "ast/function_literal.h"
#include "ast/hole.h"
#include "ast/verify_macros.h"
#include "ir/func.h"
#include "module.h"
#include "type/all.h"

std::vector<IR::Val> Evaluate(AST::Expression *expr, Context *ctx);

extern std::vector<IR::Val> global_vals;

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

bool Shadow(Declaration *decl1, Declaration *decl2, Context *ctx) {
  if ((!decl1->type->is<type::Function>() && decl1->type != type::Generic) ||
      (!decl2->type->is<type::Function>() && decl2->type != type::Generic)) {
    return true;
  }

  // If they're both functions, we have more work to do because we allow
  // overloading so long as there are no ambiguous calls.

  // TODO can we store the data in this format to begin with?
  // TODO I don't need to fully generate code here, just the heading
  // information.
  // TODO check const-decl or not.

  auto ExtractMetaData = [](auto &eval) -> std::vector<ArgumentMetaData> {
    using eval_t = std::decay_t<decltype(eval)>;
    std::vector<ArgumentMetaData> metadata;

    if constexpr (std::is_same_v<eval_t, IR::Func *>) {
      metadata.reserve(eval->args_.size());
      for (size_t i = 0; i < eval->args_.size(); ++i) {
        metadata.push_back(ArgumentMetaData{
            /*        type = */ eval->type_->input[i],
            /*        name = */ eval->args_[i].first,
            /* has_default = */ eval->args_[i].second != nullptr});
      }
      return metadata;
    } else if constexpr (std::is_same_v<eval_t, FunctionLiteral *> ||
                         std::is_same_v<eval_t, GenericFunctionLiteral *>) {
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

  auto val1 = Evaluate(decl1->init_val.get(), ctx)[0];
  if (val1.type == nullptr) { return false; }
  auto metadata1 = std::visit(ExtractMetaData, val1.value);

  auto val2 = Evaluate(decl2->init_val.get(), ctx)[0];
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

void Declaration::ClearIdDecls() {
  stage_range_ = StageRange{};
  identifier->ClearIdDecls();
  if (type_expr) { type_expr->ClearIdDecls(); }
  if (init_val) { init_val->ClearIdDecls(); }
}

bool Declaration::IsCustomInitialized() const {
  return init_val && !init_val->is<Hole>();
}

void Declaration::VerifyType(Context *ctx) {
  {
    VERIFY_STARTING_CHECK_EXPR;

    lvalue = const_ ? Assign::Const : Assign::RVal;

    if (type_expr) {
      type_expr->VerifyType(ctx);
      HANDLE_CYCLIC_DEPENDENCIES;
      limit_to(type_expr);
      if (type_expr->type != type::Type_) {
        ctx->error_log_.NotAType(type_expr.get());
        limit_to(StageRange::Nothing());

        type = type::Err;
        identifier->limit_to(StageRange::Nothing());
      } else {
        auto results = Evaluate(type_expr.get(), ctx);
        // TODO figure out if you need to generate an error here
        if (results.size() == 1) {
          type = identifier->type =
              std::get<const type::Type *>(results[0].value);
        } else {
          type = identifier->type = type::Err;
        }
      }

      identifier->stage_range_.low = StartTypeVerificationStage;
    }

    if (this->IsCustomInitialized()) {
      init_val->VerifyType(ctx);
      HANDLE_CYCLIC_DEPENDENCIES;
      limit_to(init_val);

      if (init_val->type != type::Err) {
        if (!Inferrable(init_val->type)) {
          ctx->error_log_.UninferrableType(init_val->span);
          type = identifier->type = type::Err;
          limit_to(StageRange::Nothing());
          identifier->limit_to(StageRange::Nothing());

        } else if (!type_expr) {
          identifier->stage_range_.low = StartTypeVerificationStage;
          type = identifier->type = init_val->type;
        }
      }
    }

    if (type_expr && type_expr->type == type::Type_ && init_val &&
        !init_val->is<Hole>()) {
      if (!type::CanCastImplicitly(init_val->type, type)) {
        ctx->error_log_.AssignmentTypeMismatch(identifier.get(),
                                               init_val.get());
        limit_to(StageRange::Nothing());
      }
    }

    if (!type_expr) {
      ASSERT(init_val.get() != nullptr);
      if (!init_val->is<Hole>()) {  // I := V
        if (init_val->type == type::Err) {
          type = identifier->type = type::Err;
          limit_to(StageRange::Nothing());
          identifier->limit_to(StageRange::Nothing());
        }

      } else {  // I := --
        ctx->error_log_.InferringHole(span);
        type = init_val->type = identifier->type = type::Err;
        limit_to(StageRange::Nothing());
        init_val->limit_to(StageRange::Nothing());
        identifier->limit_to(StageRange::Nothing());
      }
    }

    if (const_ && init_val) {
      if (init_val->is<Hole>()) {
        ctx->error_log_.UninitializedConstant(span);
        limit_to(StageRange::Nothing());
        return;
      } else if (init_val->lvalue != Assign::Const) {
        ASSERT(init_val->lvalue != Assign::Unset);
        ctx->error_log_.NonConstantBindingToConstantDeclaration(span);
        limit_to(StageRange::Nothing());
        return;
      }
    }

    if (type == type::Err) {
      limit_to(StageRange::Nothing());
      return;
    }

    if (identifier->is<Hole>()) {
      if (type == type::Module) {
        // TODO check shadowing against other modules?
        // TODO what if no init val is provded? what if not constant?
        ctx->mod_->embedded_modules_.push_back(
            std::get<const Module *>(Evaluate(init_val.get(), ctx)[0].value));
      } else {
        NOT_YET(type);
      }
    }
  }
  std::vector<Declaration *> decls_to_check;
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
      decls_to_check.insert(decls_to_check.end(), iter->second.begin(),
                            iter->second.end());
    }
  }

  auto iter =
      std::partition(decls_to_check.begin(), decls_to_check.end(),
                     [this](AST::Declaration *decl) { return this >= decl; });
  bool failed_shadowing = false;
  while (iter != decls_to_check.end()) {
    auto *decl = *iter;
    decl->VerifyType(ctx);
    HANDLE_CYCLIC_DEPENDENCIES;
    if (Shadow(this, decl, ctx)) {
      failed_shadowing = true;
      ctx->error_log_.ShadowingDeclaration(*this, *decl);
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
    return;
  }
}

void Declaration::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  if (type_expr) { type_expr->Validate(ctx); }
  if (init_val) { init_val->Validate(ctx); }
}

void Declaration::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  identifier->SaveReferences(scope, args);
  if (type_expr) { type_expr->SaveReferences(scope, args); }
  if (init_val) { init_val->SaveReferences(scope, args); }
}

void Declaration::contextualize(
    const Node *correspondant,
    const std::unordered_map<const Expression *, IR::Val> &replacements) {
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

void Declaration::ExtractReturns(std::vector<const Expression *> *rets) const {
  identifier->ExtractReturns(rets);
  if (type_expr) { type_expr->ExtractReturns(rets); }
  if (init_val) { init_val->ExtractReturns(rets); }
}

Declaration *Declaration::Clone() const {
  auto *result       = new Declaration;
  result->span       = span;
  result->const_     = const_;
  result->identifier = base::wrap_unique(identifier->Clone());
  result->type_expr =
      type_expr ? base::wrap_unique(type_expr->Clone()) : nullptr;
  result->init_val = init_val ? base::wrap_unique(init_val->Clone()) : nullptr;
  return result;
}

IR::Val AST::Declaration::EmitIR(Context *ctx) {
  if (const_) {
    // TODO it's custom or default initialized. cannot be uninitialized. This
    // should be verified by the type system.
    if (IsCustomInitialized()) {
      auto eval = Evaluate(init_val.get(), ctx);
      if (ctx->num_errors()) { return IR::Val::None(); }
      addr = eval[0];
    } else if (IsDefaultInitialized()) {
      NOT_YET();
    } else {
      UNREACHABLE();
    }
  } else if (scope_ == ctx->mod_->global_.get()) {
    // TODO these checks actually overlap and could be simplified.
    if (IsUninitialized(this)) {
      global_vals.emplace_back();
      global_vals.back().type = type;
      addr = IR::Val::GlobalAddr(global_vals.size() - 1, type);
    } else if (IsCustomInitialized()) {
      auto eval = Evaluate(init_val.get(), ctx);
      if (ctx->num_errors()) { return IR::Val::None(); }
      global_vals.push_back(eval[0]);
      addr = IR::Val::GlobalAddr(global_vals.size() - 1, type);

    } else if (IsDefaultInitialized()) {
      NOT_YET();
      addr = IR::Val::GlobalAddr(global_vals.size() - 1, type);

    } else if (IsInferred()) {
      NOT_YET();

    } else {
      UNREACHABLE();
    }
  } else {
    // For local variables the declaration determines where the initial value is
    // set, but the allocation has to be done much earlier. We do the allocation
    // in FunctionLiteral::EmitIR. Declaration::EmitIR is just used to set the
    // value.
    ASSERT(addr != IR::Val::None());
    ASSERT(scope_->ContainingFnScope() != nullptr);

    // TODO these checks actually overlap and could be simplified.
    if (IsUninitialized(this)) { return IR::Val::None(); }
    if (IsCustomInitialized()) {
      if (init_val->lvalue == Assign::RVal) {
        type::EmitMoveInit(init_val->type, type, init_val->EmitIR(ctx), addr,
                           ctx);
      } else {
        type::EmitCopyInit(init_val->type, type, init_val->EmitIR(ctx), addr,
                           ctx);
      }
    } else {
      type->EmitInit(addr, ctx);
    }
  }

  return addr;
}

IR::Val Declaration::EmitLVal(Context *) { UNREACHABLE(this); }
}  // namespace AST
