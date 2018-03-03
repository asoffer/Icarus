#include "ast.h"

#include "../context.h"
#include "../error/log.h"
#include "../ir/func.h"
#include "../type/type.h"

// TODO catch functions that don't return along all paths.
// TODO Join/Meet for EmptyArray <-> [0; T] (explicitly empty)? Why!?

static constexpr int ThisStage() { return 1; }

std::vector<IR::Val> Evaluate(AST::Expression *expr);
std::vector<IR::Val> Evaluate(AST::Expression *expr, Context *ctx);

#define STARTING_CHECK                                                         \
  do {                                                                         \
    ASSERT(scope_, "Need to first call assign_scope()");                       \
    if (type == Unknown) {                                                     \
      ErrorLog::CyclicDependency(this);                                        \
      type = Err;                                                              \
      limit_to(StageRange::Nothing());                                         \
    }                                                                          \
    if (type != nullptr) { return; }                                           \
    type = Unknown;                                                            \
  } while (false)

#define VALIDATE_AND_RETURN_ON_ERROR(expr)                                     \
  do {                                                                         \
    expr->Validate(ctx);                                                       \
    if (expr->type == Err) {                                                   \
      type = Err;                                                              \
      /* TODO Maybe this should be Nothing() */                                \
      limit_to(expr->stage_range_.high);                                       \
      return;                                                                  \
    }                                                                          \
  } while (false)

namespace AST {
struct ArgumentMetaData {
  Type *type;
  std::string name;
  bool has_default;
};

// TODO: This algorithm is sufficiently complicated you should combine it with
// proof of correctness and good explanation of what it does.
static bool
CommonAmbiguousFunctionCall(const std::vector<ArgumentMetaData> &data1,
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
  // yields a non-negative integer. This is because any subtraction that occurs
  // is always preceeded by an addition.
  size_t accumulator = 0;
  for (size_t i = 0; i < delta_fwd_matches.size(); ++i) {
    if (data1[i].type != data2[i].type) { break; }
    accumulator += delta_fwd_matches[i];
    if (accumulator == 0) { indices.push_back(i + 1); }
  }

  // TODO working backwards through indices should allow you to avoid having to
  // copy index2 each time and redo the same work repeatedly.
  for (auto index : indices) {
    // Everything after this index would have to be named or defaulted. named
    // values that match but with different types would have to be defaulted.
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
        // second function which haven't been named by anything in the first.
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

bool Shadow(Declaration *decl1, Declaration *decl2) {
  if ((!decl1->type->is<Function>() && decl1->type != Generic) ||
      (!decl2->type->is<Function>() && decl2->type != Generic)) {
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
      // declarations that you do not have an "= something" part. It's just the
      // "foo: bar" part. But for function arguments, we call the "= something"
      // part the default.
      return metadata;

    } else {
      UNREACHABLE();
    }
  };

  auto val1 = Evaluate(decl1->init_val.get())[0];
  if (val1.type == nullptr) { return false; }
  auto metadata1 = std::visit(ExtractMetaData, val1.value);

  auto val2 = Evaluate(decl2->init_val.get())[0];
  if (val2.type == nullptr) { return false; }
  auto metadata2 = std::visit(ExtractMetaData, val2.value);

  return CommonAmbiguousFunctionCall(metadata1, metadata2);
}

static Type *DereferenceAll(Type *t) {
  while (t->is<Pointer>()) { t = t->as<Pointer>().pointee; }
  return t;
}

// TODO move this and type-related functions below into type/type.cc?
static Type *TypeJoin(Type *lhs, Type *rhs) {
  if (lhs == rhs) { return lhs; }
  if (lhs == Err) { return rhs; } // Ignore errors
  if (rhs == Err) { return lhs; } // Ignore errors
  if (lhs == NullPtr && rhs->is<Pointer>()) { return rhs; }
  if (rhs == NullPtr && lhs->is<Pointer>()) { return lhs; }
  if (lhs->is<Pointer>() && rhs->is<Pointer>()) {
    return TypeJoin(lhs->as<Pointer>().pointee, rhs->as<Pointer>().pointee);
  } else if (lhs->is<Array>() && rhs->is<Array>()) {
    Type *result = nullptr;
    if (lhs->as<Array>().fixed_length && rhs->as<Array>().fixed_length) {
      if (lhs->as<Array>().len != rhs->as<Array>().len) { return nullptr; }
      result = TypeJoin(lhs->as<Array>().data_type, rhs->as<Array>().data_type);
      return result ? Arr(result, lhs->as<Array>().len) : result;
    } else {
      result = TypeJoin(lhs->as<Array>().data_type, rhs->as<Array>().data_type);
      return result ? Arr(result) : result;
    }
  } else if (lhs->is<Array>() && rhs == EmptyArray &&
             !lhs->as<Array>().fixed_length) {
    return lhs;
  } else if (rhs->is<Array>() && lhs == EmptyArray &&
             !rhs->as<Array>().fixed_length) {
    return rhs;
  } else if (lhs->is<Tuple>() && rhs->is<Tuple>()) {
    Tuple *lhs_tup = &lhs->as<Tuple>();
    Tuple *rhs_tup = &rhs->as<Tuple>();
    if (lhs_tup->entries.size() != rhs_tup->entries.size()) { return nullptr; }
    std::vector<Type *> joined;
    for (size_t i = 0; i < lhs_tup->entries.size(); ++i) {
      Type *result = TypeJoin(lhs_tup->entries[i], rhs_tup->entries[i]);
      if (result == nullptr) { return nullptr; }
      joined.push_back(result);
    }
    return Tup(std::move(joined));
  } else if (lhs->is<Variant>()) {
    std::vector<Type *> rhs_types;
    if (rhs->is<Variant>()) {
      rhs_types = rhs->as<Variant>().variants_;
    } else {
      rhs_types = {rhs};
    }

    auto vars = lhs->as<Variant>().variants_;
    vars.insert(vars.end(), rhs_types.begin(), rhs_types.end());
    return Var(std::move(vars));
  } else if (rhs->is<Variant>()) { // lhs is not a variant
    // TODO faster lookups? maybe not represented as a vector. at least give a
    // better interface.
    for (Type *v : rhs->as<Variant>().variants_) {
      if (lhs == v) { return rhs; }
    }
    return nullptr;
  }
  UNREACHABLE();
}

static bool CanCastImplicitly(Type *from, Type *to) {
  return TypeJoin(from, to) == to;
}

static bool Inferrable(Type *t) {
  if (t == NullPtr || t == EmptyArray) {
    return false;
  } else if (t->is<Array>()) {
    return Inferrable(t->as<Array>().data_type);
  } else if (t->is<Pointer>()) {
    return Inferrable(t->as<Pointer>().pointee);
  } else if (t->is<Tuple>()) {
    for (auto *entry : t->as<Tuple>().entries) {
      if (!Inferrable(entry)) { return false; }
    }
  } else if (t->is<Function>()) {
    return Inferrable(Tup(t->as<Function>().input)) &&
           Inferrable(Tup(t->as<Function>().output));
  }
  // TODO higher order types?
  return true;
}

std::optional<Binding> Binding::MakeUntyped(
    AST::Expression *fn_expr, const FnArgs<std::unique_ptr<Expression>> &args,
    const std::unordered_map<std::string, size_t> &index_lookup) {
  Binding result(fn_expr, index_lookup.size());
  for (size_t i = 0; i < args.pos_.size(); ++i) {
    result.exprs_[i] = std::pair(nullptr, args.pos_[i].get());
  }

  // Match the named arguments
  for (const auto & [ name, expr ] : args.named_) {
    // TODO emit an error explaining why we couldn't use this one if there was
    // a missing named argument.
    auto iter = index_lookup.find(name);
    if (iter == index_lookup.end()) { return std::nullopt; }
    result.exprs_[iter->second] = std::pair(nullptr, expr.get());
  }
  return result;
}

Binding::Binding(AST::Expression *fn_expr, size_t n)
    : fn_expr_(fn_expr),
      exprs_(n, std::pair<Type *, Expression *>(nullptr, nullptr)) {}

// We already know there can be at most one match (multiple matches would have
// been caught by shadowing), so we just return a pointer to it if it exists,
// and null otherwise.
std::optional<DispatchTable>
Call::ComputeDispatchTable(std::vector<Expression *> fn_options, Context *ctx) {
  DispatchTable table;
  for (Expression *fn_option : fn_options) {
    auto fn_val = Evaluate(fn_option)[0].value;
    if (fn_option->type->is<Function>()) {
      auto *fn_lit = std::get<FunctionLiteral *>(fn_val);

      auto maybe_binding = Binding::MakeUntyped(fn_lit, args_, fn_lit->lookup_);
      if (!maybe_binding) { continue; }
      auto binding = std::move(maybe_binding).value();

      FnArgs<Type *> call_arg_types;
      call_arg_types.pos_.resize(args_.pos_.size(), nullptr);
      for (const auto & [ key, val ] : fn_lit->lookup_) {
        if (val < args_.pos_.size()) { continue; }
        call_arg_types.named_.emplace(key, nullptr);
      }

      const auto &fn_opt_input = fn_option->type->as<Function>().input;
      for (size_t i = 0; i < binding.exprs_.size(); ++i) {
        if (binding.defaulted(i)) {
          if (fn_lit->inputs[i]->IsDefaultInitialized()) { goto next_option; }
          binding.exprs_[i].first = fn_opt_input[i];
        } else {
          Type *match =
              Type::Meet(binding.exprs_[i].second->type, fn_opt_input[i]);
          if (match == nullptr) { goto next_option; }
          binding.exprs_[i].first = fn_opt_input[i];

          if (i < call_arg_types.pos_.size()) {
            call_arg_types.pos_[i] = match;
          } else {
            auto iter =
                call_arg_types.find(fn_lit->inputs[i]->identifier->token);
            ASSERT(iter != call_arg_types.named_.end(), "");
            iter->second = match;
          }
        }
      }

      table.insert(std::move(call_arg_types), std::move(binding));

    } else if (fn_option->type == Generic) {
      auto *gen_fn_lit = std::get<GenericFunctionLiteral *>(fn_val);
      if (auto[fn_lit, binding] = gen_fn_lit->ComputeType(args_, ctx); fn_lit) {
        // TODO this is copied almost exactly from above.
        FnArgs<Type *> call_arg_types;
        call_arg_types.pos_.resize(args_.pos_.size(), nullptr);
        for (const auto & [ key, val ] : gen_fn_lit->lookup_) {
          if (val < args_.pos_.size()) { continue; }
          call_arg_types.named_.emplace(key, nullptr);
        }

        const auto &fn_opt_input = fn_lit->type->as<Function>().input;
        for (size_t i = 0; i < binding.exprs_.size(); ++i) {
          if (binding.defaulted(i)) {
            if (fn_lit->inputs[i]->IsDefaultInitialized()) { goto next_option; }
            binding.exprs_[i].first = fn_opt_input[i];
          } else {
            Type *match =
                Type::Meet(binding.exprs_[i].second->type, fn_opt_input[i]);
            if (match == nullptr) { goto next_option; }
            binding.exprs_[i].first = fn_opt_input[i];

            if (i < call_arg_types.pos_.size()) {
              call_arg_types.pos_[i] = match;
            } else {
              auto iter =
                  call_arg_types.find(fn_lit->inputs[i]->identifier->token);
              ASSERT(iter != call_arg_types.named_.end(),
                     "looking for " + fn_lit->inputs[i]->identifier->token);
              iter->second = match;
            }
          }
        }

        table.insert(std::move(call_arg_types), std::move(binding));
      } else {
        goto next_option;
      }
    } else if (fn_option->type == Err) {
      // If there's a type error, do I want to exit entirely or assume this one
      // doesn't exist? and juts goto next_option?
      return std::nullopt;

    } else {
      UNREACHABLE();
    }
  next_option:;
  }
  return std::move(table);
}

std::pair<FunctionLiteral *, Binding> GenericFunctionLiteral::ComputeType(
    const FnArgs<std::unique_ptr<Expression>> &args, Context *ctx) {
  auto maybe_binding = Binding::MakeUntyped(this, args, lookup_);
  if (!maybe_binding.has_value()) { return std::pair(nullptr, Binding{}); }
  auto binding = std::move(maybe_binding).value();

  Context new_ctx;
  Expression *expr_to_eval = nullptr;

  for (size_t i : decl_order_) {
    Type *input_type = nullptr;
    if (inputs[i]->type_expr) {
      if (auto type_result = Evaluate(inputs[i]->type_expr.get(), &new_ctx);
          !type_result.empty() && type_result[0] != IR::Val::None()) {
        input_type = std::get<Type *>(type_result[0].value);
      } else {
        ErrorLog::LogGeneric(TextSpan(), "TODO " __FILE__ ":" +
                                             std::to_string(__LINE__) + ": ");
        return std::pair(nullptr, Binding{});
      }
    } else {
      // TODO must this type have been computed already? The line below might be
      // superfluous.
      inputs[i]->init_val->Validate(&new_ctx);
      input_type = inputs[i]->init_val->type;
    }

    if (input_type == Err) { return std::pair(nullptr, Binding{}); }

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
        if (Type *match =
                Type::Meet(binding.exprs_[i].second->type, input_type);
            match == nullptr) {
          return std::pair(nullptr, Binding{});
        }
      }
    }

    if (inputs[i]->const_) {
      if (binding.defaulted(i)) {
        new_ctx.bound_constants_.emplace(
            inputs[i]->identifier->token,
            Evaluate(inputs[i].get(), &new_ctx)[0]);
      } else {
        new_ctx.bound_constants_.emplace(
            inputs[i]->identifier->token,
            Evaluate(binding.exprs_[i].second, ctx)[0]);
      }
    }

    binding.exprs_[i].first = input_type;
  }

  auto[iter, success] =
      fns_.emplace(new_ctx.bound_constants_, FunctionLiteral{});
  if (success) {
    auto &func            = iter->second;
    func.return_type_expr = base::wrap_unique(return_type_expr->Clone());
    func.inputs.reserve(inputs.size());
    for (const auto &input : inputs) {
      func.inputs.emplace_back(input->Clone());
      func.inputs.back()->arg_val = &func;
    }
    func.statements = base::wrap_unique(statements->Clone());
    func.ClearIdDecls();
    // TODO clone captures
    DoStages<0, 2>(&func, scope_, &new_ctx);
  }

  binding.fn_expr_ = &iter->second;
  return std::pair(&iter->second, binding);
}

void GenericFunctionLiteral::Validate(Context * /* ctx */) {
  STAGE_CHECK;
  STARTING_CHECK;
  lvalue = Assign::Const;
  type   = Generic; // Doable at construction.
  // TODO sort these correctly.
  decl_order_.reserve(inputs.size());
  for (size_t i = 0; i < inputs.size(); ++i) { decl_order_.push_back(i); }
}

void DispatchTable::insert(FnArgs<Type *> call_arg_types, Binding binding) {
  u64 expanded_size = 1;
  call_arg_types.Apply([&expanded_size](Type *t) {
    if (t->is<Variant>()) { expanded_size *= t->as<Variant>().size(); }
  });

  total_size_ += expanded_size;
  bindings_.emplace(std::move(call_arg_types), std::move(binding));
}

Type *Expression::VerifyTypeForDeclaration(const std::string &id_tok,
                                           Context *ctx) {
  ASSERT_NE(type, nullptr);
  if (type != Type_) {
    ErrorLog::NotAType(span, id_tok);
    return Err;
  }

  Type *t = std::get<Type *>(Evaluate(this, ctx)[0].value);

  if (t == Void) {
    ErrorLog::DeclaredVoidType(span, id_tok);
    return Err;
  }

  return t;
}

Type *Expression::VerifyValueForDeclaration(const std::string &) {
  if (type == Void) {
    ErrorLog::VoidDeclaration(span);
    return Err;
  }
  return type;
}

static bool ValidateComparisonType(Language::Operator op, Type *lhs_type,
                                   Type *rhs_type) {
  ASSERT(op == Language::Operator::Lt || op == Language::Operator::Le ||
             op == Language::Operator::Eq || op == Language::Operator::Ne ||
             op == Language::Operator::Ge || op == Language::Operator::Gt,
         "Expecting a ChainOp operator type.");

  if (lhs_type->is<Primitive>() || rhs_type->is<Primitive>() ||
      lhs_type->is<Enum>() || rhs_type->is<Enum>()) {
    if (lhs_type != rhs_type) {
      ErrorLog::LogGeneric(TextSpan(), "TODO " __FILE__ ":" +
                                           std::to_string(__LINE__) + ": ");
      return false;
    }

    if (lhs_type == Int || lhs_type == Real) { return true; }

    if (lhs_type == Code || lhs_type == Void) { return false; }
    // TODO NullPtr, String types?

    if (lhs_type == Bool || lhs_type == Char || lhs_type == Type_ ||
        (lhs_type->is<Enum>() && lhs_type->as<Enum>().is_enum_)) {
      if (op == Language::Operator::Eq || op == Language::Operator::Ne) {
        return true;
      } else {
        ErrorLog::LogGeneric(TextSpan(), "TODO " __FILE__ ":" +
                                             std::to_string(__LINE__) + ": ");
        return false;
      }
    } else if (lhs_type->is<Enum>() && !lhs_type->as<Enum>().is_enum_) {
      return true;
    }
  }

  if (lhs_type->is<Pointer>()) {
    if (lhs_type != rhs_type) {
      ErrorLog::LogGeneric(TextSpan(), "TODO " __FILE__ ":" +
                                           std::to_string(__LINE__) + ": ");
      return false;
    } else if (op != Language::Operator::Eq && op != Language::Operator::Ne) {
      ErrorLog::LogGeneric(TextSpan(), "TODO " __FILE__ ":" +
                                           std::to_string(__LINE__) + ": ");
      return false;
    } else {
      return true;
    }
  }

  // TODO there are many errors that might occur here and we should export all
  // of them. For instance, I might try to compare two arrays of differing fixed
  // lengths with '<'. We should not exit early, but rather say that you can't
  // use '<' and that the lengths are different.
  if (lhs_type->is<Array>()) {
    if (rhs_type->is<Array>()) {
      if (op != Language::Operator::Eq && op != Language::Operator::Ne) {
        ErrorLog::LogGeneric(TextSpan(), "TODO " __FILE__ ":" +
                                             std::to_string(__LINE__) + ": ");
        return false;
      }

      Array *lhs_array = &lhs_type->as<Array>();
      Array *rhs_array = &lhs_type->as<Array>();

      // TODO what if data types are equality comparable but not equal?
      if (lhs_array->data_type != rhs_array->data_type) {
        ErrorLog::LogGeneric(TextSpan(), "TODO " __FILE__ ":" +
                                             std::to_string(__LINE__) + ": ");
        return false;
      }

      if (lhs_array->fixed_length && rhs_array->fixed_length) {
        if (lhs_array->len == rhs_array->len) {
          return true;
        } else {
          ErrorLog::LogGeneric(TextSpan(), "TODO " __FILE__ ":" +
                                               std::to_string(__LINE__) + ": ");
          return false;
        }
      } else {
        return true; // If at least one array length is variable, we should be
                     // allowed to compare them.
      }
    } else {
      ErrorLog::LogGeneric(TextSpan(), "TODO " __FILE__ ":" +
                                           std::to_string(__LINE__) + ": ");
      return false;
    }
  }

  ErrorLog::LogGeneric(TextSpan(),
                       "TODO " __FILE__ ":" + std::to_string(__LINE__) + ": ");
  return false;
}

void Terminal::Validate(Context *) {
  STAGE_CHECK;
  lvalue = Assign::Const;
}

void Identifier::Validate(Context *ctx) {
  STAGE_CHECK;
  STARTING_CHECK;

  if (decl == nullptr) {
    auto potential_decls = scope_->AllDeclsWithId(token);

    if (potential_decls.size() != 1) {
      if (potential_decls.empty()) {
        ctx->error_log_.UndeclaredIdentifier(this);
      } else {
        // TODO is this reachable? Or does shadowing cover this case?
        ErrorLog::LogGeneric(this->span, "TODO " __FILE__ ":" +
                                             std::to_string(__LINE__) + ": ");
      }
      type = Err;
      limit_to(StageRange::Nothing());
      return;
    }

    if (potential_decls[0]->const_ && potential_decls[0]->arg_val != nullptr &&
        potential_decls[0]->arg_val->is<GenericFunctionLiteral>()) {
      if (auto iter =
              ctx->bound_constants_.find(potential_decls[0]->identifier->token);
          iter != ctx->bound_constants_.end()) {
        potential_decls[0]->arg_val = scope_->ContainingFnScope()->fn_lit;
      } else {
        ctx->error_log_.UndeclaredIdentifier(this);
        type = Err;
        limit_to(StageRange::Nothing());
        return;
      }
    }
    decl = potential_decls[0];
  }

  if (!decl->const_ && (span.start.line_num < decl->span.start.line_num ||
                        (span.start.line_num == decl->span.start.line_num &&
                         span.start.offset < decl->span.start.offset))) {
    ctx->error_log_.DeclOutOfOrder(decl, this);
    limit_to(StageRange::NoEmitIR());
  }

  type   = decl->type;
  lvalue = decl->lvalue == Assign::Const ? Assign::Const : Assign::LVal;

  if (lvalue != Assign::Const) {
    // For everything else we iterate from the scope of this identifier up to
    // the scope in which it was declared checking that along the way that it's
    // a block scope.
    for (auto scope_ptr = scope_; scope_ptr != decl->scope_;
         scope_ptr      = scope_ptr->parent) {
      if (scope_ptr->is<FnScope>()) {
        scope_ptr->as<FnScope>().fn_lit->captures.insert(decl);
      } else if (scope_ptr->is<ExecScope>()) {
        continue;
      } else {
        ErrorLog::LogGeneric(this->span, "TODO " __FILE__ ":" +
                                             std::to_string(__LINE__) + ": ");
        limit_to(StageRange::NoEmitIR());
        return;
      }
    }
  }
}

void Hole::Validate(Context *) {
  STAGE_CHECK;
  lvalue = Assign::Const;
}

void Binop::Validate(Context *ctx) {
  STAGE_CHECK;
  STARTING_CHECK;

  lhs->Validate(ctx);
  rhs->Validate(ctx);
  if (lhs->type == Err || rhs->type == Err) {
    type = Err;
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
    ErrorLog::InvalidAssignment(span, lhs->lvalue);
    limit_to(StageRange::Nothing());

  } else if (op == Operator::Cast || op == Operator::Index) {
    lvalue = rhs->lvalue;
  } else if (lhs->lvalue == Assign::Const && rhs->lvalue == Assign::Const) {
    lvalue = Assign::Const;
  } else {
    lvalue = Assign::RVal;
  }

  // TODO if lhs is reserved?
  if (op == Operator::Assign) {
    if (CanCastImplicitly(rhs->type, lhs->type)) {
      type = Void;
    } else {
      ErrorLog::LogGeneric(this->span, "TODO " __FILE__ ":" +
                                           std::to_string(__LINE__) + ": ");
      type = Err;
      limit_to(StageRange::NoEmitIR());
    }
    return;
  }

  switch (op) {
  case Operator::Rocket: {
    // TODO rocket encountered outside case statement.
    UNREACHABLE();
  } break;
  case Operator::Index: {
    type = Err;
    if (lhs->type == String) {
      if (rhs->type != Int) {
        ErrorLog::InvalidStringIndex(span, rhs->type);
        limit_to(StageRange::NoEmitIR());
      }
      type = Char; // Assuming it's a char, even if the index type was wrong.
      return;
    } else if (!lhs->type->is<Array>()) {
      if (rhs->type->is<RangeType>()) {
        ErrorLog::SlicingNonArray(span, lhs->type);
      } else {
        ErrorLog::IndexingNonArray(span, lhs->type);
      }
      limit_to(StageRange::NoEmitIR());
      return;
    } else if (rhs->type->is<RangeType>()) {
      type = Slice(&lhs->type->as<Array>());
      break;
    } else {
      type = lhs->type->as<Array>().data_type;

      // TODO allow slice indexing
      if (rhs->type == Int) { break; }
      ErrorLog::NonIntegralArrayIndex(span, rhs->type);
      limit_to(StageRange::NoEmitIR());
      return;
    }
  } break;
  case Operator::Dots: {
    if (lhs->type == Int && rhs->type == Int) {
      type = Range(Int);

    } else if (lhs->type == Char && rhs->type == Char) {
      type = Range(Char);

    } else {
      ErrorLog::InvalidRangeTypes(span, lhs->type, rhs->type);
      limit_to(StageRange::Nothing());
      return;
    }
  } break;
  case Operator::XorEq: {
    if (lhs->type == Bool && rhs->type == Bool) {
      type = Bool;
    } else if (lhs->type->is<Enum>() && rhs->type == lhs->type &&
               !lhs->type->as<Enum>().is_enum_) {
      type = lhs->type;
    } else {
      type = Err;
      // TODO could be bool or enum.
      ErrorLog::XorEqNeedsBool(span);
      limit_to(StageRange::Nothing());
      return;
    }
  } break;
  case Operator::AndEq: {
    if (lhs->type == Bool && rhs->type == Bool) {
      type = Bool;
    } else if (lhs->type->is<Enum>() && rhs->type == lhs->type &&
               !lhs->type->as<Enum>().is_enum_) {
      type = lhs->type;
    } else {
      type = Err;
      // TODO could be bool or enum.
      ErrorLog::AndEqNeedsBool(span);
      limit_to(StageRange::Nothing());
      return;
    }
  } break;
  case Operator::OrEq: {
    if (lhs->type == Bool && rhs->type == Bool) {
      type = Bool;
    } else if (lhs->type->is<Enum>() && rhs->type == lhs->type &&
               !lhs->type->as<Enum>().is_enum_) {
      type = lhs->type;
    } else {
      type = Err;
      // TODO could be bool or enum.
      ErrorLog::OrEqNeedsBool(span);
      limit_to(StageRange::Nothing());
      return;
    }
  } break;

#define CASE(OpName, op_name, symbol, ret_type)                                \
  case Operator::OpName: {                                                     \
    if ((lhs->type == Int && rhs->type == Int) ||                              \
        (lhs->type == Real && rhs->type == Real) ||                            \
        (lhs->type == Code && rhs->type == Code)) {                            \
      /* TODO Code should only be valid for Add, not Sub, etc */               \
      type = ret_type;                                                         \
    } else {                                                                   \
      /* Store a vector containing the valid matches */                        \
      std::vector<Declaration *> matched_op_name;                              \
                                                                               \
      /* TODO this linear search is probably not ideal.   */                   \
      scope_->ForEachDecl([&ctx, &matched_op_name](Declaration *decl) {        \
        if (decl->identifier->token == "__" op_name "__") {                    \
          decl->Validate(ctx);                                                 \
          matched_op_name.push_back(decl);                                     \
        }                                                                      \
      });                                                                      \
                                                                               \
      Declaration *correct_decl = nullptr;                                     \
      for (auto &decl : matched_op_name) {                                     \
        if (!decl->type->is<Function>()) { continue; }                         \
        auto *fn_type = &decl->type->as<Function>();                           \
        if (fn_type->input != std::vector{lhs->type, rhs->type}) { continue; } \
        /* If you get here, you've found a match. Hope there is only one       \
         * TODO if there is more than one, log them all and give a good        \
         * *error message. For now, we just fail */                            \
        if (correct_decl) {                                                    \
          ErrorLog::AlreadyFoundMatch(span, symbol, lhs->type, rhs->type);     \
          type = Err;                                                          \
        } else {                                                               \
          correct_decl = decl;                                                 \
        }                                                                      \
      }                                                                        \
      if (!correct_decl) {                                                     \
        type = Err;                                                            \
        ErrorLog::NoKnownOverload(span, symbol, lhs->type, rhs->type);         \
        limit_to(StageRange::Nothing());                                       \
        return;                                                                \
      } else if (type != Err) {                                                \
        type = Tup(correct_decl->type->as<Function>().output);                 \
      }                                                                        \
    }                                                                          \
  } break;

    CASE(Add, "add", "+", lhs->type);
    CASE(Sub, "sub", "-", lhs->type);
    CASE(Div, "div", "/", lhs->type);
    CASE(Mod, "mod", "%", lhs->type);
    CASE(AddEq, "add_eq", "+=", Void);
    CASE(SubEq, "sub_eq", "-=", Void);
    CASE(MulEq, "mul_eq", "*=", Void);
    CASE(DivEq, "div_eq", "/=", Void);
    CASE(ModEq, "mod_eq", "%=", Void);
#undef CASE

  // Mul is done separately because of the function composition
  case Operator::Mul: {
    if ((lhs->type == Int && rhs->type == Int) ||
        (lhs->type == Real && rhs->type == Real)) {
      type = lhs->type;

    } else if (lhs->type->is<Function>() && rhs->type->is<Function>()) {
      auto *lhs_fn = &lhs->type->as<Function>();
      auto *rhs_fn = &rhs->type->as<Function>();
      if (rhs_fn->output == lhs_fn->input) {
        type = Func(rhs_fn->input, lhs_fn->output);

      } else {
        type = Err;
        ErrorLog::NonComposableFunctions(span);
        limit_to(StageRange::Nothing());
        return;
      }

    } else {
      for (auto scope_ptr = scope_; scope_ptr; scope_ptr = scope_ptr->parent) {
        auto id_ptr = scope_ptr->IdHereOrNull("__mul__");
        if (!id_ptr) { continue; }
      }

      auto fn_type = scope_->FunctionTypeReferencedOrNull(
          "__mul__", {lhs->type, rhs->type});
      if (fn_type) {
        type = Tup(fn_type->as<Function>().output);
      } else {
        type = Err;
        ErrorLog::NoKnownOverload(span, "*", lhs->type, rhs->type);
        limit_to(StageRange::Nothing());
        return;
      }
    }
  } break;
  case Operator::Arrow: {
    if (lhs->type != Type_) {
      type = Err;
      ErrorLog::NonTypeFunctionInput(span);
      limit_to(StageRange::Nothing());
      return;
    }
    if (rhs->type != Type_) {
      type = Err;
      ErrorLog::NonTypeFunctionOutput(span);
      limit_to(StageRange::Nothing());
      return;
    }

    if (type != Err) { type = Type_; }

  } break;
  default: UNREACHABLE();
  }
}

void Call::Validate(Context *ctx) {
  STAGE_CHECK;
  STARTING_CHECK;

  bool all_const = true;
  args_.Apply([&ctx, &all_const, this](auto &arg) {
    arg->Validate(ctx);
    if (arg->type == Err) { this->type = Err; }
    all_const &= arg->lvalue == Assign::Const;
  });

  lvalue = all_const ? Assign::Const : Assign::RVal;
  if (type == Err) {
    limit_to(StageRange::Nothing());
    return;
  }

  // Identifiers need special handling due to overloading. compile-time
  // constants need special handling because they admit named arguments. These
  // concepts should be orthogonal.
  std::vector<Expression *> fn_options;
  if (fn_->is<Identifier>()) {
    const auto &token = fn_->as<Identifier>().token;
    for (auto *scope_ptr = scope_; scope_ptr; scope_ptr = scope_ptr->parent) {
      if (scope_ptr->shadowed_decls_.find(token) !=
          scope_ptr->shadowed_decls_.end()) {
        // The declaration of this identifier is shadowed so it will be
        // difficult to give meaningful error messages. Bailing out.
        // TODO Come up with a good way to give decent error messages anyway (at
        // least in some circumstances. For instance, there may be overlap here,
        // but it may not be relevant to the function call at hand. If, for
        // example, one call takes an A | B and the other takes a B | C, but
        // here we wish to validate a call for just a C, it's safe to do so
        // here.
        type = Err;
        limit_to(StageRange::Nothing());
        return;
      }

      auto iter = scope_ptr->decls_.find(token);
      if (iter == scope_ptr->decls_.end()) { continue; }
      for (const auto &decl : iter->second) {
        decl->Validate(ctx);
        if (decl->type == Err) {
          limit_to(decl->stage_range_.high);
          return;
        }
        fn_options.push_back(decl);
      }
    }
  } else {
    fn_->Validate(ctx);
    fn_options.push_back(fn_.get());
  }

  if (auto maybe_table = ComputeDispatchTable(std::move(fn_options), ctx)) {
    dispatch_table_ = std::move(maybe_table.value());
  } else {
    // Failure because we can't emit IR for the function so the error was
    // previously logged.
    type = Err;
    limit_to(StageRange::Nothing());
    return;
  }

  u64 expanded_size = 1;
  args_.Apply([&expanded_size](auto &arg) {
    if (arg->type->template is<Variant>()) {
      expanded_size *= arg->type->template as<Variant>().size();
    }
  });

  if (dispatch_table_.total_size_ != expanded_size) {
    LOG << "Failed to find a match for everything. ("
        << dispatch_table_.total_size_ << " vs " << expanded_size << ")";
    type = fn_->type = Err;
    limit_to(StageRange::Nothing());
    return;
  }

  if (fn_->is<Identifier>()) {
    // fn_'s type should never be considered beacuse it could be one of many
    // different things. 'Void' just indicates that it has been computed (i.e.,
    // not 0x0 or Unknown) and that there was no error in doing so (i.e., not
    // Err).
    fn_->type = Void;
  }

  std::vector<Type *> out_types;
  out_types.reserve(dispatch_table_.bindings_.size());
  for (const auto & [ key, val ] : dispatch_table_.bindings_) {
    auto &outs = val.fn_expr_->type->as<Function>().output;
    ASSERT_LE(outs.size(), 1u);
    out_types.push_back(outs.empty() ? Void : outs[0]);
  }
  type = Var(std::move(out_types));
}

// TODO Declaration is responsible for the type verification of it's identifier?
// TODO rewrite/simplify
void Declaration::Validate(Context *ctx) {
  STAGE_CHECK;
  STARTING_CHECK;

  lvalue = const_ ? Assign::Const : Assign::RVal;

  if (type_expr) {
    type_expr->Validate(ctx);
    if (type_expr->type == Err) { type = Err; }
    limit_to(type_expr);
  }
  if (init_val) {
    init_val->Validate(ctx);
    if (init_val->type == Err) {
      type = Err;
    } else {
      if (init_val->lvalue != Assign::Const && const_) {
        ErrorLog::LogGeneric(this->span, "TODO " __FILE__ ":" +
                                             std::to_string(__LINE__) + "\n");
        limit_to(StageRange::Nothing());
      } else {
        limit_to(init_val);
      }
    }
  }
  if (type == Err) {
    type = type_expr ? type_expr->type : Err;
    return;
  }

  // There are eight cases for the form of a declaration.
  //   1a. I: T         or    1b. I :: T
  //   2a. I := V       or    2b. I ::= V
  //   3a. I: T = V     or    3b. I :: T = V
  //   4a. I: T = --    or    4b. I :: T = -- (illegal)
  //
  // Here 'I' stands for "identifier". This is the identifier being declared.
  // 'T' stands for "type", the type of the identifier being declared.
  // 'V' stands for "value", the initial value of the identifier being declared.
  // The double-colon (b) cases are those where the values being declared must
  // be known at compile-time. The single-colon (a) cases the values being
  // declared need only be known at run-time. In case 4a and 4b, the --
  // indicates that the value is not specified. The only valid operations to
  // perform on such an identifier is to assign to it. This might be useful in
  // 4a for optimzation purposes. In the case of 4b it is outright illegal and
  // an error is thrown by the compiler.

  if (IsDefaultInitialized()) {
    ASSERT_NE(type_expr.get(), nullptr);
    type = type_expr->VerifyTypeForDeclaration(identifier->token, ctx);
  } else if (IsInferred()) {
    type = init_val->VerifyValueForDeclaration(identifier->token);

    if (!Inferrable(type)) {
      // Some types are only present in the type-system to make handling easier
      // for us, but really cannot be inferred. For example, almost dealing
      // with 'null':
      //   foo := [null, null]  // Not valid
      //   foo: [2; &int] = [null, null] // Okay. Type explicitly stated.
      ErrorLog::UninferrableType(span);
      type = Err;
      limit_to(StageRange::Nothing());
    }

  } else if (IsCustomInitialized()) {
    type = type_expr->VerifyTypeForDeclaration(identifier->token, ctx);
    auto init_val_type = init_val->VerifyValueForDeclaration(identifier->token);

    if (type == Err) {
      type = init_val_type;
    } else if (!CanCastImplicitly(init_val_type, type)) {
      ErrorLog::AssignmentTypeMismatch(span, type, init_val_type);
    }

    if (!type->is<Pointer>() && init_val_type == NullPtr) {
      ErrorLog::LogGeneric(this->span, "TODO " __FILE__ ":" +
                                           std::to_string(__LINE__) + ": ");
      limit_to(StageRange::NoEmitIR());
    }

  } else if (IsUninitialized()) {
    if (const_) {
      ErrorLog::LogGeneric(this->span, "TODO " __FILE__ ":" +
                                           std::to_string(__LINE__) + ": ");
      limit_to(StageRange::NoEmitIR());
    }

    type = type_expr->VerifyTypeForDeclaration(identifier->token, ctx);
    init_val->type = type;

  } else {
    UNREACHABLE();
  }

  identifier->Validate(ctx);
  limit_to(identifier);

  if (type == Err) {
    limit_to(StageRange::Nothing());
    return;
  }

  // TODO is this the right time to complete the struct definition?
  if (type->is<Struct>()) { type->as<Struct>().CompleteDefinition(ctx); }

  // TODO Either guarantee that higher scopes have all declarations declared and
  // verified first, or check both in the upwards and downwards direction for
  // shadowing.
  bool failed_shadowing = false;
  for (auto *decl : scope_->AllDeclsWithId(identifier->token)) {
    if (decl == this) { continue; }
    if (this < decl) {
      // Pick one arbitrary but consistent ordering of the pair to check because
      // at each Declaration verification, we look both up and down the scope
      // tree.
      decl->Validate(ctx);
      if (Shadow(this, decl)) {
        failed_shadowing = true;
        ErrorLog::ShadowingDeclaration(*this, *decl);
      }
    }
  }
  auto iter = scope_->child_decls_.find(identifier->token);
  if (iter != scope_->child_decls_.end()) {
    for (auto *decl : iter->second) {
      if (this < decl) {
        // Pick one arbitrary but consistent ordering of the pair to check
        // because at each Declaration verification, we look both up and down
        // the scope tree.
        decl->Validate(ctx);
        if (Shadow(this, decl)) {
          failed_shadowing = true;
          ErrorLog::ShadowingDeclaration(*this, *decl);
        }
      }
    }
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

void InDecl::Validate(Context *ctx) {
  STAGE_CHECK;
  STARTING_CHECK;
  container->Validate(ctx);
  limit_to(container);

  lvalue = Assign::RVal;

  if (container->type == Void) {
    type             = Err;
    identifier->type = Err;
    ErrorLog::TypeIteration(span);
    limit_to(StageRange::Nothing());
    return;
  }

  if (container->type->is<Array>()) {
    type = container->type->as<Array>().data_type;

  } else if (container->type->is<SliceType>()) {
    type = container->type->as<SliceType>().array_type->data_type;

  } else if (container->type->is<RangeType>()) {
    type = container->type->as<RangeType>().end_type;

  } else if (container->type == Type_) {
    auto t = std::get<Type *>(Evaluate(container.get())[0].value);
    if (t->is<Enum>()) { type = t; }

  } else {
    ErrorLog::IndeterminantType(span);
    type = Err;
    limit_to(StageRange::Nothing());
  }

  identifier->type = type;
  identifier->Validate(ctx);
}

void Statements::Validate(Context *ctx) {
  STAGE_CHECK;
  for (auto &stmt : content_) {
    stmt->Validate(ctx);
    limit_to(stmt);
  }
}

void CodeBlock::Validate(Context *) {}

void Unop::Validate(Context *ctx) {
  STAGE_CHECK;
  STARTING_CHECK;
  VALIDATE_AND_RETURN_ON_ERROR(operand);

  using Language::Operator;

  if (op != Operator::At && op != Operator::And) {
    lvalue = operand->lvalue == Assign::Const ? Assign::Const : Assign::LVal;
  }

  switch (op) {
  case Operator::Eval: type = operand->type; break;
  case Operator::Require: type = Void; break;
  case Operator::Generate: type = Void; break;
  case Operator::Free: {
    if (!operand->type->is<Pointer>()) {
      ErrorLog::UnopTypeFail("Attempting to free an object of type `" +
                                 operand->type->to_string() + "`.",
                             this);
      limit_to(StageRange::NoEmitIR());
    }
    type = Void;
  } break;
  case Operator::Print: {
    if (operand->type == Void) {
      ErrorLog::UnopTypeFail(
          "Attempting to print an expression with type `void`.", this);
      limit_to(StageRange::NoEmitIR());
    }
    type = Void;
  } break;
  case Operator::Return: {
    if (operand->type == Void) {
      ErrorLog::UnopTypeFail(
          "Attempting to return an expression which has type `void`.", this);
      limit_to(StageRange::NoEmitIR());
    }
    type = Void;
  } break;
  case Operator::At: {
    lvalue = Assign::LVal;
    if (operand->type->is<Pointer>()) {
      type = operand->type->as<Pointer>().pointee;

    } else {
      ErrorLog::UnopTypeFail(
          "Attempting to dereference an expression of type `" +
              operand->type->to_string() + "`.",
          this);
      type = Err;
      limit_to(StageRange::Nothing());
    }
  } break;
  case Operator::And: {
    switch (operand->lvalue) {
    case Assign::Const: [[fallthrough]];
    case Assign::RVal:
      ErrorLog::InvalidAddress(span, operand->lvalue);
      lvalue = Assign::RVal;
      break;
    case Assign::LVal: break;
    case Assign::Unset: UNREACHABLE();
    }
    type = Ptr(operand->type);
  } break;
  case Operator::Mul: {
    limit_to(operand);
    if (operand->type != Type_) {
      ErrorLog::LogGeneric(this->span, "TODO " __FILE__ ":" +
                                           std::to_string(__LINE__) + ": ");
      type = Err;
      limit_to(StageRange::Nothing());
    } else {
      type = Type_;
    }
  } break;
  case Operator::Sub: {
    if (operand->type == Int || operand->type == Real) {
      type = operand->type;

    } else if (operand->type->is<Struct>()) {
      for (auto scope_ptr = scope_; scope_ptr; scope_ptr = scope_ptr->parent) {
        auto id_ptr = scope_ptr->IdHereOrNull("__neg__");
        if (!id_ptr) { continue; }

        id_ptr->Validate(ctx);
      }

      auto t = scope_->FunctionTypeReferencedOrNull("__neg__",
                                                    std::vector{operand->type});
      if (t) {
        type = Tup(t->as<Function>().output);
      } else {
        ErrorLog::UnopTypeFail("Type `" + operand->type->to_string() +
                                   "` has no unary negation operator.",
                               this);
        type = Err;
        limit_to(StageRange::Nothing());
      }
    } else {
      ErrorLog::UnopTypeFail("Type `" + operand->type->to_string() +
                                 "` has no unary negation operator.",
                             this);
      type = Err;
      limit_to(StageRange::Nothing());
    }
  } break;
  case Operator::Dots: {
    if (operand->type == Int || operand->type == Char) {
      type = Range(operand->type);
    } else {
      ErrorLog::InvalidRangeType(span, type);
      type = Err;
      limit_to(StageRange::Nothing());
    }
  } break;
  case Operator::Not: {
    if (operand->type == Bool) {
      type = Bool;
    } else {
      ErrorLog::UnopTypeFail("Attempting to apply the logical negation "
                             "operator (!) to an expression of type `" +
                                 operand->type->to_string() + "`.",
                             this);
      type = Err;
      limit_to(StageRange::Nothing());
    }
  } break;
  case Operator::Needs: {
    type = Void;
    if (operand->type != Bool) {
      ctx->error_log_.PreconditionNeedsBool(this);
      limit_to(StageRange::NoEmitIR());
    }
  } break;
  case Operator::Ensure: {
    type = Void;
    if (operand->type != Bool) {
      ctx->error_log_.PostconditionNeedsBool(this);
      limit_to(StageRange::NoEmitIR());
    }
  } break;
  case Operator::Pass: type = operand->type; break;
  default: UNREACHABLE(*this);
  }
  limit_to(operand);
}

void Access::Validate(Context *ctx) {
  STAGE_CHECK;
  STARTING_CHECK;
  VALIDATE_AND_RETURN_ON_ERROR(operand);
  lvalue = (operand->type->is<Array>() &&
            operand->type->as<Array>().fixed_length && member_name == "size")
               ? Assign::Const
               : operand->lvalue;

  auto base_type = DereferenceAll(operand->type);
  if (base_type->is<Array>()) {
    if (member_name == "size") {
      type = Int;
    } else {
      ErrorLog::MissingMember(span, member_name, base_type);
      type = Err;
      limit_to(StageRange::Nothing());
    }
  } else if (base_type == Type_) {
    Type *evaled_type = std::get<Type *>(Evaluate(operand.get())[0].value);
    if (evaled_type->is<Enum>()) {
      // Regardless of whether we can get the value, it's clear that this is
      // supposed to be a member so we should emit an error but carry on
      // assuming that this is an element of that enum type.
      type = evaled_type;
      if (evaled_type->as<Enum>().IntValueOrFail(member_name) ==
          std::numeric_limits<size_t>::max()) {
        ErrorLog::MissingMember(span, member_name, evaled_type);
        limit_to(StageRange::NoEmitIR());
      }
    }
  } else if (base_type->is<Struct>()) {
    auto struct_type = static_cast<Struct *>(base_type);
    struct_type->CompleteDefinition(ctx);

    auto member_type = struct_type->field(member_name);
    if (member_type != nullptr) {
      type = member_type;

    } else {
      ErrorLog::MissingMember(span, member_name, base_type);
      type = Err;
      limit_to(StageRange::Nothing());
    }
  } else if (base_type->is<Primitive>() || base_type->is<Function>()) {
    ErrorLog::MissingMember(span, member_name, base_type);
    type = Err;
    limit_to(StageRange::Nothing());
  }
}

void ChainOp::Validate(Context *ctx) {
  STAGE_CHECK;
  STARTING_CHECK;
  bool found_err = false;

  lvalue = Assign::Const;
  for (auto &expr : exprs) {
    expr->Validate(ctx);
    limit_to(expr);
    if (expr->type == Err) { found_err = true; }
    if (expr->lvalue != Assign::Const) { lvalue = Assign::RVal; }
  }
  if (found_err) {
    type = Err;
    limit_to(StageRange::Nothing());
    return;
  }

  // TODO Can we recover from errors here? Should we?

  // Safe to just check first because to be on the same chain they must all have
  // the same precedence, and ^, &, and | uniquely hold a given precedence.
  if (ops[0] == Language::Operator::Or || ops[0] == Language::Operator::And ||
      ops[0] == Language::Operator::Xor) {
    bool failed = false;
    for (const auto &expr : exprs) {
      if (expr->type != exprs[0]->type) {
        ErrorLog::LogGeneric(TextSpan(), "TODO " __FILE__ ":" +
                                             std::to_string(__LINE__) + ": ");
        failed = true;
      }
    }

    type = exprs[0]->type;

    if (exprs[0]->type != Bool &&
        !(exprs[0]->type == Type_ && ops[0] == Language::Operator::Or) &&
        (!exprs[0]->type->is<Enum>() || exprs[0]->type->as<Enum>().is_enum_)) {
      ErrorLog::LogGeneric(TextSpan(), "TODO " __FILE__ ":" +
                                           std::to_string(__LINE__) + ": ");
      if (failed) {
        limit_to(StageRange::Nothing());
        return;
      }
    }

  } else {
    for (size_t i = 0; i < exprs.size() - 1; ++i) {
      if (!ValidateComparisonType(ops[i], exprs[i]->type, exprs[i + 1]->type)) {
        type = Err; // Errors exported by ValidateComparisonType
      }
    }
    if (type == Err) { limit_to(StageRange::Nothing()); }
    type = Bool;
  }
}

void CommaList::Validate(Context *ctx) {
  STAGE_CHECK;
  STARTING_CHECK;
  lvalue = Assign::Const;
  for (auto &expr : exprs) {
    expr->Validate(ctx);
    limit_to(expr);
    if (expr->type == Err) { type = Err; }
    if (expr->lvalue != Assign::Const) { lvalue = Assign::RVal; }
  }
  if (type == Err) {
    limit_to(StageRange::Nothing());
    return;
  }

  // TODO this is probably not a good way to do it.  If the tuple consists of a
  // list of types, it should be interpretted as a
  // type itself rather than a tuple. This is a limitation in your support of
  // full tuples.
  bool all_types = true;
  std::vector<Type *> type_vec(exprs.size(), nullptr);

  size_t position = 0;
  for (const auto &expr : exprs) {
    type_vec[position] = expr->type;
    all_types &= (expr->type == Type_);
    ++position;
  }
  // TODO got to have a better way to make tuple types i think
  type = all_types ? Type_ : Tup(type_vec);
}

void ArrayLiteral::Validate(Context *ctx) {
  STAGE_CHECK;
  STARTING_CHECK;

  lvalue = Assign::Const;
  if (elems.empty()) {
    type = EmptyArray;
    return;
  }

  for (auto &elem : elems) {
    elem->Validate(ctx);
    limit_to(elem);
    if (elem->lvalue != Assign::Const) { lvalue = Assign::RVal; }
  }

  Type *joined = Err;
  for (auto &elem : elems) {
    joined = TypeJoin(joined, elem->type);
    if (joined == nullptr) { break; }
  }

  if (joined == nullptr) {
    // Types couldn't be joined. Emit an error
    ErrorLog::InconsistentArrayType(span);
    type = Err;
    limit_to(StageRange::Nothing());
  } else if (joined == Err) {
    type = Err; // There were no valid types anywhere in the array
    limit_to(StageRange::Nothing());
  } else {
    type = Arr(joined, elems.size());
  }
}

void ArrayType::Validate(Context *ctx) {
  STAGE_CHECK;
  STARTING_CHECK;
  lvalue = Assign::Const;

  length->Validate(ctx);
  limit_to(length);
  data_type->Validate(ctx);
  limit_to(data_type);

  type = Type_;

  if (length->is<Hole>()) { return; }
  if (length->lvalue != Assign::Const || data_type->lvalue != Assign::Const) {
    lvalue = Assign::RVal;
  }

  if (length->type != Int) {
    ErrorLog::ArrayIndexType(span);
    limit_to(StageRange::NoEmitIR());
  }
}

void Case::Validate(Context *ctx) {
  STAGE_CHECK;
  STARTING_CHECK;
  for (auto & [ key, val ] : key_vals) {
    key->Validate(ctx);
    val->Validate(ctx);
    limit_to(key);
    limit_to(val);

    // TODO do you want case-statements to be usable as lvalues? Currently they
    // are not.
    if (key->lvalue != Assign::Const || val->lvalue != Assign::Const) {
      lvalue = Assign::RVal;
    }
  }

  std::unordered_map<Type *, size_t> value_types;

  for (auto & [ key, val ] : key_vals) {
    if (key->type == Err) {
      key->type = Bool;

    } else if (key->type != Bool) {
      ErrorLog::CaseLHSBool(span, key->span, key->type);
      key->type = Bool;
    }

    if (val->type == Err) {
      type = Err;
      limit_to(StageRange::Nothing());
      return;
    }
    ++value_types[val->type];
  }

  if (value_types.size() != 1) {

    // In order to give a message saying that a particular type is incorrect, we
    // need either
    // * 1/2 of them have the same type
    // * 1/4 of them have the same type, and no other type hits > 1/8
    //
    // NOTE: These numbers were chosen somewhat arbitrarily.

    size_t max_size = 0;
    size_t min_size = key_vals.size();
    Type *max_type  = nullptr;
    for (const auto & [ key, val ] : value_types) {
      if (val > max_size) {
        max_size = val;
        max_type = key;
      }

      if (val < min_size) { min_size = val; }
    }

    if (2 * max_size > key_vals.size() ||
        (4 * max_size > key_vals.size() && 8 * min_size < key_vals.size())) {
      ErrorLog::CaseTypeMismatch(this, max_type);
      type = max_type;
    } else {
      ErrorLog::CaseTypeMismatch(this);
      type = Err;
      limit_to(StageRange::Nothing());
    }
  } else {
    type = value_types.begin()->first;
  }
}

void FunctionLiteral::Validate(Context *ctx) {
  STAGE_CHECK;
  STARTING_CHECK;

  lvalue = Assign::Const;
  return_type_expr->Validate(ctx);
  limit_to(return_type_expr);

  if (ctx->num_errors() > 0) {
    type = Err;
    limit_to(StageRange::Nothing());
    return;
  }

  // TODO should named return types be required?
  auto rets = Evaluate(
      [&]() {
        if (!return_type_expr->is<Declaration>()) {
          return return_type_expr.get();
        }

        auto *decl_return = &return_type_expr->as<Declaration>();
        if (decl_return->IsInferred()) { NOT_YET(); }

        return decl_return->type_expr.get();
      }(),
      ctx);

  if (rets.empty()) {
    type = Err;
    limit_to(StageRange::Nothing());
    return;
  }

  auto &ret_type_val = rets[0];

  // TODO must this really be undeclared?
  if (ret_type_val == IR::Val::None() /* TODO Error() */) {
    ErrorLog::IndeterminantType(return_type_expr.get());
    type = Err;
    limit_to(StageRange::Nothing());
  } else if (ret_type_val.type != Type_) {
    ErrorLog::NotAType(return_type_expr.get(), return_type_expr->type);
    type = Err;
    limit_to(StageRange::Nothing());
    return;
  } else if (std::get<Type *>(ret_type_val.value) == Err) {
    type = Err;
    limit_to(StageRange::Nothing());
    return;
  }

  for (auto &input : inputs) { input->Validate(ctx); }

  // TODO poison on input Err?

  Type *ret_type    = std::get<Type *>(ret_type_val.value);
  size_t num_inputs = inputs.size();
  std::vector<Type *> input_type_vec;
  input_type_vec.reserve(inputs.size());
  for (const auto &input : inputs) { input_type_vec.push_back(input->type); }

  type = Func(input_type_vec, ret_type);
}

void For::Validate(Context *ctx) {
  STAGE_CHECK;
  for (auto &iter : iterators) {
    iter->Validate(ctx);
    limit_to(iter);
  }
  statements->Validate(ctx);
  limit_to(statements);
}

void Jump::Validate(Context *) {
  STAGE_CHECK;
  // TODO made this slightly wrong
  auto scope_ptr = scope_;
  while (scope_ptr && scope_ptr->is<ExecScope>()) {
    auto exec_scope_ptr = &scope_ptr->as<ExecScope>();
    if (exec_scope_ptr->can_jump) {
      scope = exec_scope_ptr;
      return;
    }
    scope_ptr = exec_scope_ptr->parent;
  }
  ErrorLog::JumpOutsideLoop(span);
  limit_to(StageRange::NoEmitIR());
}

void ScopeNode::Validate(Context *ctx) {
  STAGE_CHECK;
  STARTING_CHECK;

  lvalue = Assign::RVal;

  scope_expr->Validate(ctx);
  limit_to(scope_expr);
  if (expr != nullptr) {
    expr->Validate(ctx);
    limit_to(expr);
  }
  stmts->Validate(ctx);
  limit_to(stmts);

  if (!scope_expr->type->is<Scope_Type>()) {
    ErrorLog::InvalidScope(scope_expr->span, scope_expr->type);
    type = Err;
    limit_to(StageRange::Nothing());
    return;
  }

  // TODO verify it uses the fields correctly
  type = Void; // TODO can this evaluate to anything?
}

void ScopeLiteral::Validate(Context *ctx) {
  STAGE_CHECK;
  STARTING_CHECK;
  lvalue                            = Assign::Const;
  bool cannot_proceed_due_to_errors = false;
  if (!enter_fn) {
    cannot_proceed_due_to_errors = true;
    ErrorLog::LogGeneric(this->span, "TODO " __FILE__ ":" +
                                         std::to_string(__LINE__) + ": ");
  }

  if (!exit_fn) {
    cannot_proceed_due_to_errors = true;
    ErrorLog::LogGeneric(this->span, "TODO " __FILE__ ":" +
                                         std::to_string(__LINE__) + ": ");
  }

  if (cannot_proceed_due_to_errors) {
    limit_to(StageRange::Nothing());
    return;
  }

  VALIDATE_AND_RETURN_ON_ERROR(enter_fn);
  VALIDATE_AND_RETURN_ON_ERROR(exit_fn);

  if (!enter_fn->type->is<Function>()) {
    cannot_proceed_due_to_errors = true;
    ErrorLog::LogGeneric(this->span, "TODO " __FILE__ ":" +
                                         std::to_string(__LINE__) + ": ");
  }

  if (!exit_fn->type->is<Function>()) {
    cannot_proceed_due_to_errors = true;
    ErrorLog::LogGeneric(this->span, "TODO " __FILE__ ":" +
                                         std::to_string(__LINE__) + ": ");
  }

  if (cannot_proceed_due_to_errors) {
    limit_to(StageRange::Nothing());
  } else {
    type = ScopeType(Tup(enter_fn->type->as<Function>().input));
  }
}
} // namespace AST

#undef VALIDATE_AND_RETURN_ON_ERROR
#undef STARTING_CHECK
