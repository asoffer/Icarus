#include "ast.h"

#include <algorithm>

#include "ast/hole.h"
#include "error/log.h"
#include "ir/func.h"
#include "type/all.h"
#include "module.h"
#include "verify_macros.h"

// TODO catch functions that don't return along all paths.
// TODO Join/Meet for type::EmptyArray <-> [0; T] (explicitly empty)? Why!?

IR::Val ErrorFunc();
IR::Val AsciiFunc();
IR::Val OrdFunc();

using base::check::Is; 
using base::check::Not; 

std::vector<IR::Val> Evaluate(AST::Expression *expr, Context *ctx);

namespace AST {
struct ArgumentMetaData {
  const type::Type *type;
  std::string name;
  bool has_default;
};

// TODO: This algorithm is sufficiently complicated you should combine it
// with proof of correctness and good explanation of what it does.
static bool CommonAmbiguousFunctionCall(
    const std::vector<ArgumentMetaData> &data1,
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

static bool Shadow(Declaration *decl1, Declaration *decl2, Context *ctx) {
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

static const type::Type *DereferenceAll(const type::Type *t) {
  while (t->is<type::Pointer>()) { t = t->as<type::Pointer>().pointee; }
  return t;
}

static bool CanCastImplicitly(const type::Type *from, const type::Type *to) {
  return type::Join(from, to) == to;
}

// TODO return a reason why this is not inferrable for better error messages.
static bool Inferrable(const type::Type *t) {
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

std::optional<Binding> Binding::MakeUntyped(
    AST::Expression *fn_expr, const FnArgs<Expression *> &args,
    const std::unordered_map<std::string, size_t> &index_lookup) {
  Binding result(fn_expr, index_lookup.size());
  for (size_t i = 0; i < args.pos_.size(); ++i) {
    result.exprs_[i] = std::pair(nullptr, args.pos_[i]);
  }

  // Match the named arguments
  for (const auto & [ name, expr ] : args.named_) {
    // TODO emit an error explaining why we couldn't use this one if there
    // was a missing named argument.
    auto iter = index_lookup.find(name);
    if (iter == index_lookup.end()) { return std::nullopt; }
    result.exprs_[iter->second] = std::pair(nullptr, expr);
  }
  return result;
}

Binding::Binding(AST::Expression *fn_expr, size_t n)
    : fn_expr_(fn_expr),
      exprs_(n, std::pair<type::Type *, Expression *>(nullptr, nullptr)) {}

std::pair<FunctionLiteral *, Binding> GenericFunctionLiteral::ComputeType(
    const FnArgs<Expression *> &args, Context *ctx) {
  auto maybe_binding = Binding::MakeUntyped(this, args, lookup_);
  if (!maybe_binding.has_value()) { return std::pair(nullptr, Binding{}); }
  auto binding = std::move(maybe_binding).value();

  Context new_ctx(ctx->mod_);
  BoundConstants bound_constants;
  new_ctx.bound_constants_ = &bound_constants;
  Expression *expr_to_eval = nullptr;

  for (size_t i : decl_order_) {
    inputs[i]->VerifyType(&new_ctx);
    if (inputs[i]->type == type::Err) { return std::pair(nullptr, Binding{}); }

    if (binding.defaulted(i) && inputs[i]->IsDefaultInitialized()) {
      // TODO maybe send back an explanation of why this didn't match. Or
      // perhaps continue to get better diagnostics?
      return std::pair(nullptr, Binding{});
    } else {
      if (inputs[i]->const_ && !binding.defaulted(i) &&
          binding.exprs_[i].second->lvalue != Assign::Const) {
        return std::pair(nullptr, Binding{});
      }

      if (!binding.defaulted(i)) {
        if (auto *match =
                type::Meet(binding.exprs_[i].second->type, inputs[i]->type);
            match == nullptr) {
          return std::pair(nullptr, Binding{});
        }
      }
    }

    if (inputs[i]->const_) {
      bound_constants.emplace(
          inputs[i]->identifier->token,
          (binding.defaulted(i) ? Evaluate(inputs[i].get(), &new_ctx)
                                : Evaluate(binding.exprs_[i].second, ctx))[0]);
    }

    binding.exprs_[i].first = inputs[i]->type;
  }

  auto[iter, success] =
      fns_.emplace(std::move(bound_constants), FunctionLiteral{});
   new_ctx.bound_constants_ = &iter->first;
  if (success) {
    auto &func                 = iter->second;
    func.bound_constants_      = &iter->first;
    func.return_type_inferred_ = return_type_inferred_;

    func.inputs.reserve(inputs.size());
    for (const auto &input : inputs) {
      func.inputs.emplace_back(input->Clone());
      func.inputs.back()->arg_val = &func;
    }

    func.outputs.reserve(outputs.size());
    for (const auto &output : outputs) {
      func.outputs.emplace_back(output->Clone());
      if (output->is<Declaration>()) {
        func.outputs.back()->as<Declaration>().arg_val = &func;
      }
    }

    func.statements = base::wrap_unique(statements->Clone());
    func.ClearIdDecls();

    func.assign_scope(scope_);
    func.VerifyType(&new_ctx);
    func.Validate(&new_ctx);
    func.EmitIR(&new_ctx);
    
    if (new_ctx.num_errors() > 0) {
      // TODO figure out the call stack of generic function requests and then
      // print the relevant parts.
      std::cerr << "While generating code for generic function:\n";
      new_ctx.DumpErrors();
      // TODO delete all the data held by iter->second
      return std::pair(nullptr, Binding{});
    }
  }

  binding.fn_expr_ = &iter->second;
  return std::pair(&iter->second, binding);
}

void GenericFunctionLiteral::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  // TODO sort these correctly.
  decl_order_.reserve(inputs.size());
  for (size_t i = 0; i < inputs.size(); ++i) { decl_order_.push_back(i); }
}

void DispatchTable::insert(FnArgs<const type::Type *> call_arg_types,
                           Binding binding, size_t expanded_size) {
  if (expanded_size == std::numeric_limits<size_t>::max()) {
    expanded_size = 1;
    call_arg_types.Apply([&expanded_size](const type::Type *t) {
      if (t->is<type::Variant>()) {
        expanded_size *= t->as<type::Variant>().size();
      }
    });
  }

  total_size_ += expanded_size;
  bindings_.emplace(std::move(call_arg_types), std::move(binding));
}

std::vector<Expression *> FunctionOptions(const std::string &token,
                                          Scope *scope, Context *ctx);

const type::Type *SetDispatchTable(const FnArgs<Expression *> &args,
                                   std::vector<Expression *> fn_options,
                                   AST::DispatchTable *dispatch_table,
                                   Context *ctx);

void Binop::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;

  lhs->VerifyType(ctx);
  HANDLE_CYCLIC_DEPENDENCIES;
  rhs->VerifyType(ctx);
  HANDLE_CYCLIC_DEPENDENCIES;
  if (lhs->type == type::Err || rhs->type == type::Err) {
    type = type::Err;
    limit_to(lhs);
    limit_to(rhs);
    return;
  }

  using Language::Operator;
  if (lhs->lvalue != Assign::LVal &&
      (op == Operator::Assign || op == Operator::OrEq ||
       op == Operator::XorEq || op == Operator::AndEq ||
       op == Operator::AddEq || op == Operator::SubEq ||
       op == Operator::MulEq || op == Operator::DivEq ||
       op == Operator::ModEq)) {
    switch (lhs->lvalue) {
      case Assign::Unset: UNREACHABLE();
      case Assign::Const: ctx->error_log_.AssigningToConstant(span); break;
      case Assign::RVal: ctx->error_log_.AssigningToTemporary(span); break;
      case Assign::LVal: UNREACHABLE();
    }
    limit_to(StageRange::Nothing());
  } else if (op == Operator::Index) {
    lvalue = rhs->lvalue;
  } else if (lhs->lvalue == Assign::Const && rhs->lvalue == Assign::Const) {
    lvalue = Assign::Const;
  } else {
    lvalue = Assign::RVal;
  }

  // TODO if lhs is reserved?
  if (op == Operator::Assign) {
    if (lhs->type->is<type::Tuple>()) {
      if (rhs->type->is<type::Tuple>()) {
        const auto &lhs_entries_ = lhs->type->as<type::Tuple>().entries_;
        const auto &rhs_entries_ = rhs->type->as<type::Tuple>().entries_;

        if (lhs_entries_.size() != rhs_entries_.size()) {
          NOT_YET("error message");
        } else {
          for (size_t i = 0; i < lhs_entries_.size(); ++i) {
            if (!CanCastImplicitly(rhs_entries_[i], lhs_entries_[i])) {
              ErrorLog::LogGeneric(
                  this->span,
                  "TODO " __FILE__ ":" + std::to_string(__LINE__) + ": ");
              limit_to(StageRange::NoEmitIR());
            }
          }
        }
      } else {
        LOG << lhs;
        LOG << rhs;
        NOT_YET("error message");
      }
    } else {
      if (rhs->type->is<type::Tuple>()){
        LOG << lhs;
        LOG << rhs;
        NOT_YET("error message");
      } else {
        if (!CanCastImplicitly(rhs->type, lhs->type)) {
          ErrorLog::LogGeneric(this->span, "TODO " __FILE__ ":" +
                                               std::to_string(__LINE__) + ": ");
          limit_to(StageRange::NoEmitIR());
        }
      }
    }

    return;
  }

  switch (op) {
    case Operator::Index: {
      type = type::Err;
      if (lhs->type == type::String) {
        if (rhs->type != type::Int) {
          ErrorLog::InvalidStringIndex(span, rhs->type);
          limit_to(StageRange::NoEmitIR());
        }
        type = type::Char;  // Assuming it's a char, even if the index type was
                            // wrong.
        return;
      } else if (!lhs->type->is<type::Array>()) {
        ErrorLog::IndexingNonArray(span, lhs->type);
        limit_to(StageRange::NoEmitIR());
        return;
      } else {
        type = lhs->type->as<type::Array>().data_type;

        if (rhs->type == type::Int) { break; }
        ErrorLog::NonIntegralArrayIndex(span, rhs->type);
        limit_to(StageRange::NoEmitIR());
        return;
      }
    } break;
    case Operator::Dots: NOT_YET();
    case Operator::XorEq: {
      if (lhs->type == type::Bool && rhs->type == type::Bool) {
        type = type::Bool;
      } else if (lhs->type->is<type::Enum>() && rhs->type == lhs->type &&
                 !lhs->type->as<type::Enum>().is_enum_) {
        type = lhs->type;
      } else {
        type = type::Err;
        // TODO could be bool or enum.
        ctx->error_log_.XorEqNeedsBool(span);
        limit_to(StageRange::Nothing());
        return;
      }
    } break;
    case Operator::AndEq: {
      if (lhs->type == type::Bool && rhs->type == type::Bool) {
        type = type::Bool;
      } else if (lhs->type->is<type::Enum>() && rhs->type == lhs->type &&
                 !lhs->type->as<type::Enum>().is_enum_) {
        type = lhs->type;
      } else {
        type = type::Err;
        // TODO could be bool or enum.
        ctx->error_log_.AndEqNeedsBool(span);
        limit_to(StageRange::Nothing());
        return;
      }
    } break;
    case Operator::OrEq: {
      if (lhs->type == type::Bool && rhs->type == type::Bool) {
        type = type::Bool;
      } else if (lhs->type->is<type::Enum>() && rhs->type == lhs->type &&
                 !lhs->type->as<type::Enum>().is_enum_) {
        type = lhs->type;
      } else {
        type = type::Err;
        // TODO could be bool or enum.
        ctx->error_log_.OrEqNeedsBool(span);
        limit_to(StageRange::Nothing());
        return;
      }
    } break;

#define CASE(OpName, symbol, ret_type)                                         \
  case Operator::OpName: {                                                     \
    if ((lhs->type == type::Int && rhs->type == type::Int) ||                  \
        (lhs->type == type::Real && rhs->type == type::Real) ||                \
        (lhs->type == type::Code && rhs->type == type::Code)) {                \
      /* TODO type::Code should only be valid for Add, not Sub, etc */         \
      type = ret_type;                                                         \
    } else {                                                                   \
      FnArgs<Expression *> args;                                               \
      args.pos_ = std::vector{lhs.get(), rhs.get()};                           \
      type      = SetDispatchTable(args, FunctionOptions(symbol, scope_, ctx), \
                              &dispatch_table_, ctx);                          \
      ASSERT(type, Not(Is<type::Tuple>()));                                    \
      if (type == type::Err) { limit_to(StageRange::Nothing()); }              \
    }                                                                          \
  } break;

      CASE(Add, "+", lhs->type);
      CASE(Sub, "-", lhs->type);
      CASE(Div, "/", lhs->type);
      CASE(Mod, "%", lhs->type);
      CASE(AddEq, "+=", type::Void);
      CASE(SubEq, "-=", type::Void);
      CASE(MulEq, "*=", type::Void);
      CASE(DivEq, "/=", type::Void);
      CASE(ModEq, "%=", type::Void);
#undef CASE

    // Mul is done separately because of the function composition
    case Operator::Mul: {
      if ((lhs->type == type::Int && rhs->type == type::Int) ||
          (lhs->type == type::Real && rhs->type == type::Real)) {
        type = lhs->type;

      } else if (lhs->type->is<type::Function>() &&
                 rhs->type->is<type::Function>()) {
        auto *lhs_fn = &lhs->type->as<type::Function>();
        auto *rhs_fn = &rhs->type->as<type::Function>();
        if (rhs_fn->output == lhs_fn->input) {
          type = type::Func({rhs_fn->input}, {lhs_fn->output});

        } else {
          type = type::Err;
          ctx->error_log_.NonComposableFunctions(span);
          limit_to(StageRange::Nothing());
          return;
        }

      } else {
        FnArgs<Expression *> args;
        args.pos_  = std::vector{lhs.get(), rhs.get()};
        type       = SetDispatchTable(args, FunctionOptions("*", scope_, ctx),
                                &dispatch_table_, ctx);
        ASSERT(type, Not(Is<type::Tuple>()));
        if (type == type::Err) { limit_to(StageRange::Nothing()); }
      }
    } break;
    case Operator::Arrow: {
      if (lhs->type != type::Type_) {
        type = type::Err;
        ctx->error_log_.NonTypeFunctionInput(span);
        limit_to(StageRange::Nothing());
        return;
      }
      if (rhs->type != type::Type_) {
        type = type::Err;
        ctx->error_log_.NonTypeFunctionOutput(span);
        limit_to(StageRange::Nothing());
        return;
      }

      if (type != type::Err) { type = type::Type_; }

    } break;
    default: UNREACHABLE();
  }
}

bool Declaration::IsCustomInitialized() const {
  return init_val && !init_val->is<Hole>();
}
bool Declaration::IsUninitialized() const {
  return init_val && init_val->is<Hole>();
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
      if (!CanCastImplicitly(init_val->type, type)) {
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
    auto iter = scope_->child_decls_.find(identifier->token);

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

void Access::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  VERIFY_AND_RETURN_ON_ERROR(operand);
  lvalue =
      (operand->type->is<type::Array>() &&
       operand->type->as<type::Array>().fixed_length && member_name == "size")
          ? Assign::Const
          : operand->lvalue;

  auto base_type = DereferenceAll(operand->type);
  if (base_type->is<type::Array>()) {
    if (member_name == "size") {
      type = type::Int;
    } else {
      ErrorLog::MissingMember(span, member_name, base_type);
      type = type::Err;
      limit_to(StageRange::Nothing());
    }
  } else if (base_type == type::Type_) {
    auto *evaled_type =
        std::get<const type::Type *>(Evaluate(operand.get(), ctx)[0].value);
    if (evaled_type->is<type::Enum>()) {
      // Regardless of whether we can get the value, it's clear that this is
      // supposed to be a member so we should emit an error but carry on
      // assuming that this is an element of that enum type.
      type = evaled_type;
      if (evaled_type->as<type::Enum>().IntValueOrFail(member_name) ==
          std::numeric_limits<size_t>::max()) {
        ErrorLog::MissingMember(span, member_name, evaled_type);
        limit_to(StageRange::NoEmitIR());
      }
    }
  } else if (base_type->is<type::Struct>()) {
    const auto *member = base_type->as<type::Struct>().field(member_name);
    if (member != nullptr) {
      type = member->type;

    } else {
      ErrorLog::MissingMember(span, member_name, base_type);
      type = type::Err;
      limit_to(StageRange::Nothing());
    }
  } else if (base_type == type::Module) {
    auto module =
        std::get<const Module *>(Evaluate(operand.get(), ctx)[0].value);
    type = module->GetType(member_name);
    if (type == nullptr) {
      ErrorLog::LogGeneric(
          TextSpan(), "TODO " __FILE__ ":" + std::to_string(__LINE__) + ": ");
      type = type::Err;
      limit_to(StageRange::Nothing());
    }

  } else if (base_type->is<type::Primitive>() ||
             base_type->is<type::Function>()) {
    ErrorLog::MissingMember(span, member_name, base_type);
    type = type::Err;
    limit_to(StageRange::Nothing());
  }
}

void ChainOp::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  bool found_err = false;

  lvalue = Assign::Const;
  for (auto &expr : exprs) {
    expr->VerifyType(ctx);
    HANDLE_CYCLIC_DEPENDENCIES;
    limit_to(expr);
    if (expr->type == type::Err) { found_err = true; }
    if (expr->lvalue != Assign::Const) { lvalue = Assign::RVal; }
  }
  if (found_err) {
    type = type::Err;
    limit_to(StageRange::Nothing());
    return;
  }

  // TODO Can we recover from errors here? Should we?

  // Safe to just check first because to be on the same chain they must all have
  // the same precedence, and ^, &, and | uniquely hold a given precedence.
  switch (ops[0]) {
    case Language::Operator::Or:
    case Language::Operator::And:
    case Language::Operator::Xor: {
      bool failed = false;
      for (const auto &expr : exprs) {
        if (expr->type != exprs[0]->type) {
          ErrorLog::LogGeneric(TextSpan(), "TODO " __FILE__ ":" +
                                               std::to_string(__LINE__) + ": ");
          failed = true;
        }
      }

      type = exprs[0]->type;

      if (exprs[0]->type != type::Bool &&
          !(exprs[0]->type == type::Type_ &&
            ops[0] == Language::Operator::Or) &&
          (!exprs[0]->type->is<type::Enum>() ||
           exprs[0]->type->as<type::Enum>().is_enum_)) {
        ErrorLog::LogGeneric(
            TextSpan(), "TODO " __FILE__ ":" + std::to_string(__LINE__) + ": ");
        if (failed) {
          limit_to(StageRange::Nothing());
          return;
        }
      }

      return;
    } break;
    default: {
      ASSERT(exprs.size() >= 2u);
      for (size_t i = 0; i < exprs.size() - 1; ++i) {
        const type::Type *lhs_type = exprs[i]->type;
        const type::Type *rhs_type = exprs[i + 1]->type;
        if (lhs_type->is<type::Struct>() || rhs_type->is<type::Struct>()) {
          // TODO struct is wrong. generally user-defined (could be array of
          // struct too, or perhaps a variant containing a struct?) need to
          // figure out the details here.
          const char *token = nullptr;
          switch (ops[i]) {
            case Language::Operator::Lt: token = "<"; break;
            case Language::Operator::Le: token = "<="; break;
            case Language::Operator::Eq: token = "=="; break;
            case Language::Operator::Ne: token = "!="; break;
            case Language::Operator::Ge: token = ">="; break;
            case Language::Operator::Gt: token = ">"; break;
            default: UNREACHABLE();
          }

          FnArgs<Expression *> args;
          args.pos_ = std::vector{exprs[i].get(), exprs[i + 1].get()};
          type = SetDispatchTable(args, FunctionOptions(token, scope_, ctx),
                                  &dispatch_tables_[i], ctx);
          ASSERT(type, Not(Is<type::Tuple>()));
          if (type == type::Err) { limit_to(StageRange::Nothing()); }
        } else {
          if (lhs_type != rhs_type) {
            NOT_YET(lhs_type, " ", rhs_type);
          } else {
            auto cmp = lhs_type->Comparator();

            switch (ops[i]) {
              case Language::Operator::Eq:
              case Language::Operator::Ne: {
                switch (cmp) {
                  case type::Cmp::Order:
                  case type::Cmp::Equality: continue;
                  case type::Cmp::None:
                    type = type::Err;
                    ErrorLog::LogGeneric(
                        TextSpan(),
                        "TODO " __FILE__ ":" + std::to_string(__LINE__) + ": ");
                }
              } break;
              case Language::Operator::Lt:
              case Language::Operator::Le:
              case Language::Operator::Ge:
              case Language::Operator::Gt: {
                switch (cmp) {
                  case type::Cmp::Order: continue;
                  case type::Cmp::Equality:
                  case type::Cmp::None:
                    type = type::Err;
                    ErrorLog::LogGeneric(
                        TextSpan(),
                        "TODO " __FILE__ ":" + std::to_string(__LINE__) + ": ");
                }
              } break;
              default: UNREACHABLE("Expecting a ChainOp operator type.");
            }
          }
        }
      }

      if (type == type::Err) { limit_to(StageRange::Nothing()); }
      type = type::Bool;
    }
  }
}

void CommaList::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  // TODO actually compute value category
  lvalue = Assign::LVal;
  for (auto &expr : exprs) {
    expr->VerifyType(ctx);
    HANDLE_CYCLIC_DEPENDENCIES;
    limit_to(expr);
    if (expr->type == type::Err) { type = type::Err; }
  }
  if (type == type::Err) {
    limit_to(StageRange::Nothing());
    return;
  } else {
    std::vector<const type::Type *> entries;
    entries.reserve(exprs.size());
    for (const auto &expr : exprs) { entries.push_back(expr->type); }
    type = type::Tup(std::move(entries));
  }
}

void FunctionLiteral::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  lvalue = Assign::Const;
  for (auto &input : inputs) {
    input->VerifyType(ctx);
    HANDLE_CYCLIC_DEPENDENCIES;
  }

  for (auto &output : outputs) {
    output->VerifyType(ctx);
    HANDLE_CYCLIC_DEPENDENCIES;
  }
  
  if (ctx->num_errors() > 0) {
    type = type::Err;
    limit_to(StageRange::Nothing());
    return;
  }

  if (!return_type_inferred_) {
    std::vector<const type::Type *> input_type_vec;
    input_type_vec.reserve(inputs.size());
    for (const auto &input : inputs) { input_type_vec.push_back(input->type); }

    // TODO should named return types be required?
    std::vector<IR::Val> out_vals;
    out_vals.reserve(outputs.size());
    for (const auto &out : outputs) {
      auto result = Evaluate(
          [&]() {
            if (!out->is<Declaration>()) { return out.get(); }

            auto *out_decl = &out->as<Declaration>();
            if (out_decl->IsInferred()) { NOT_YET(); }
            return out_decl->type_expr.get();
          }(),
          ctx);
      ASSERT(result.size() == 1u);
      out_vals.push_back(std::move(result)[0]);
    }

    std::vector<const type::Type*> ret_types;
    ret_types.reserve(out_vals.size());
    for (size_t i= 0; i < out_vals.size(); ++i) {
      const auto& out = out_vals[i];
      if (out == IR::Val::None() /* TODO Error() */) {
        ctx->error_log_.IndeterminantType(outputs[i]->span);
        type = type::Err;
        limit_to(StageRange::Nothing());
      } else if (out.type != type::Type_) {
        ctx->error_log_.NotAType(outputs[i].get());
        type = type::Err;
        limit_to(StageRange::Nothing());
        continue;
      } else if (auto *ret_type = std::get<const type::Type *>(out.value);
                 ret_type == type::Err) {
        type = type::Err;
        limit_to(StageRange::Nothing());
        continue;
      } else {
        ret_types.push_back(ret_type);
      }
    }

    type = type::Func(std::move(input_type_vec), std::move(ret_types));
  } else {
    Validate(ctx);
  }
}

void ScopeLiteral::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  lvalue = Assign::Const;
  bool cannot_proceed_due_to_errors = false;
  if (!enter_fn) {
    cannot_proceed_due_to_errors = true;
    ErrorLog::LogGeneric(
        this->span, "TODO " __FILE__ ":" + std::to_string(__LINE__) + ": ");
  }

  if (!exit_fn) {
    cannot_proceed_due_to_errors = true;
    ErrorLog::LogGeneric(
        this->span, "TODO " __FILE__ ":" + std::to_string(__LINE__) + ": ");
  }

  if (cannot_proceed_due_to_errors) {
    limit_to(StageRange::Nothing());
    return;
  }

  VERIFY_AND_RETURN_ON_ERROR(enter_fn);
  VERIFY_AND_RETURN_ON_ERROR(exit_fn);

  if (!enter_fn->type->is<type::Function>()) {
    cannot_proceed_due_to_errors = true;
    ErrorLog::LogGeneric(
        this->span, "TODO " __FILE__ ":" + std::to_string(__LINE__) + ": ");
  }

  if (!exit_fn->type->is<type::Function>()) {
    cannot_proceed_due_to_errors = true;
    ErrorLog::LogGeneric(
        this->span, "TODO " __FILE__ ":" + std::to_string(__LINE__) + ": ");
  }

  if (cannot_proceed_due_to_errors) {
    limit_to(StageRange::Nothing());
  } else {
    type = type::Scp(enter_fn->type->as<type::Function>().input);
  }
}

void StructLiteral::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
  lvalue = Assign::Const;
  type = type::Type_;
}
}  // namespace AST
