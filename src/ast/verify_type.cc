#include "ast.h"

#include <algorithm>

#include "context.h"
#include "error/log.h"
#include "ir/func.h"
#include "type/all.h"

// TODO catch functions that don't return along all paths.
// TODO Join/Meet for type::EmptyArray <-> [0; T] (explicitly empty)? Why!?

IR::Val ErrorFunc();
IR::Val AsciiFunc();
IR::Val OrdFunc();

using base::check::Is; 
using base::check::Not; 

void ScheduleModule(const Source::Name &src);

std::vector<IR::Val> Evaluate(AST::Expression *expr, Context *ctx);

#define HANDLE_CYCLIC_DEPENDENCIES                                             \
  do {                                                                         \
    if (ctx->cyc_dep_vec_ == nullptr) { break; }                               \
    if constexpr (std::is_same_v<decltype(this), Identifier *>) {              \
      auto *this_as_id = reinterpret_cast<Identifier *>(this);                 \
      if (!ctx->cyc_dep_vec_->empty() &&                                       \
          this_as_id == ctx->cyc_dep_vec_->front()) {                          \
        ctx->cyc_dep_vec_ = nullptr;                                           \
      } else {                                                                 \
        ctx->cyc_dep_vec_->push_back(this_as_id);                              \
      }                                                                        \
    } else if constexpr (std::is_same_v<decltype(this), Declaration *>) {      \
      auto *this_as_id =                                                       \
          reinterpret_cast<Declaration *>(this)->identifier.get();             \
      if (!ctx->cyc_dep_vec_->empty() &&                                       \
          this_as_id == ctx->cyc_dep_vec_->front()) {                          \
        ctx->cyc_dep_vec_ = nullptr;                                           \
      } else {                                                                 \
        ctx->cyc_dep_vec_->push_back(this_as_id);                              \
      }                                                                        \
    }                                                                          \
    type = type::Err;                                                          \
    limit_to(StageRange::Nothing());                                           \
    return;                                                                    \
  } while (false)

#define STARTING_CHECK                                                         \
  base::defer defer_##__LINE__(                                                \
      [this]() { this->stage_range_.low = DoneTypeVerificationStage; });       \
  if (stage_range_.high < StartTypeVerificationStage) { return; }              \
  if (stage_range_.low >= DoneTypeVerificationStage) { return; }               \
  stage_range_.low = StartTypeVerificationStage

#define STARTING_CHECK_EXPR                                                    \
  base::defer defer_##__LINE__(                                                \
      [this]() { this->stage_range_.low = DoneTypeVerificationStage; });       \
  if (stage_range_.high < StartTypeVerificationStage) { return; }              \
  if (stage_range_.low >= DoneTypeVerificationStage) { return; }               \
  if (stage_range_.low == StartTypeVerificationStage) {                        \
    ctx->cyc_dep_vec_ = ctx->error_log_.CyclicDependency();                    \
    HANDLE_CYCLIC_DEPENDENCIES;                                                \
  }                                                                            \
  stage_range_.low = StartTypeVerificationStage

#define VERIFY_AND_RETURN_ON_ERROR(expr)                                     \
  do {                                                                         \
    expr->VerifyType(ctx);                                                     \
    HANDLE_CYCLIC_DEPENDENCIES;                                                \
    if (expr->type == type::Err) {                                             \
      type = type::Err;                                                        \
      /* TODO Maybe this should be Nothing() */                                \
      limit_to(expr->stage_range_.high);                                       \
      return;                                                                  \
    }                                                                          \
  } while (false)

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

// We already know there can be at most one match (multiple matches would
// have been caught by shadowing), so we just return a pointer to it if it
// exists, and null otherwise.
static std::optional<DispatchTable> ComputeDispatchTable(
    const FnArgs<Expression *> &args, std::vector<Expression *> fn_options,
    Context *ctx) {
  DispatchTable table;
  for (Expression *fn_option : fn_options) {
    // TODO the reason you fail to generate what you want is because right here
    // you are calling evaluate which is problematic for recursive functions
    auto vals = Evaluate(fn_option, ctx);
    if (vals.empty() || vals[0] == IR::Val::None()) { continue; }
    auto &fn_val = vals[0].value;
    if (fn_option->type->is<type::Function>()) {
      auto *fn_lit = std::get<FunctionLiteral *>(fn_val);

      auto maybe_binding = Binding::MakeUntyped(fn_lit, args, fn_lit->lookup_);
      if (!maybe_binding) { continue; }
      auto binding = std::move(maybe_binding).value();

      FnArgs<const type::Type *> call_arg_types;
      call_arg_types.pos_.resize(args.pos_.size(), nullptr);
      for (const auto & [ key, val ] : fn_lit->lookup_) {
        if (val < args.pos_.size()) { continue; }
        call_arg_types.named_.emplace(key, nullptr);
      }

      const auto &fn_opt_input = fn_option->type->as<type::Function>().input;
      for (size_t i = 0; i < binding.exprs_.size(); ++i) {
        if (binding.defaulted(i)) {
          if (fn_lit->inputs[i]->IsDefaultInitialized()) { goto next_option; }
          binding.exprs_[i].first = fn_opt_input[i];
        } else {
          const type::Type *match =
              type::Meet(binding.exprs_[i].second->type, fn_opt_input[i]);
          if (match == nullptr) { goto next_option; }
          binding.exprs_[i].first = fn_opt_input[i];

          if (i < call_arg_types.pos_.size()) {
            call_arg_types.pos_[i] = match;
          } else {
            auto iter =
                call_arg_types.find(fn_lit->inputs[i]->identifier->token);
            ASSERT(iter != call_arg_types.named_.end());
            iter->second = match;
          }
        }
      }

      table.insert(std::move(call_arg_types), std::move(binding));

    } else if (fn_option->type == type::Generic) {
      auto *gen_fn_lit = std::get<GenericFunctionLiteral *>(fn_val);
      if (auto[fn_lit, binding] = gen_fn_lit->ComputeType(args, ctx); fn_lit) {
        // TODO this is copied almost exactly from above.
        FnArgs<const type::Type *> call_arg_types;
        call_arg_types.pos_.resize(args.pos_.size(), nullptr);
        for (const auto & [ key, val ] : gen_fn_lit->lookup_) {
          if (val < args.pos_.size()) { continue; }
          call_arg_types.named_.emplace(key, nullptr);
        }

        const auto &fn_opt_input = fn_lit->type->as<type::Function>().input;
        for (size_t i = 0; i < binding.exprs_.size(); ++i) {
          if (binding.defaulted(i)) {
            if (fn_lit->inputs[i]->IsDefaultInitialized()) { goto next_option; }
            binding.exprs_[i].first = fn_opt_input[i];
          } else {
            const type::Type *match =
                type::Meet(binding.exprs_[i].second->type, fn_opt_input[i]);
            if (match == nullptr) { goto next_option; }
            binding.exprs_[i].first = fn_opt_input[i];

            if (i < call_arg_types.pos_.size()) {
              call_arg_types.pos_[i] = match;
            } else {
              auto iter =
                  call_arg_types.find(fn_lit->inputs[i]->identifier->token);
              ASSERT(iter != call_arg_types.named_.end());
              iter->second = match;
            }
          }
        }

        table.insert(std::move(call_arg_types), std::move(binding));
      } else {
        goto next_option;
      }
    } else if (fn_option->type == type::Type_) {
      ASSERT(args.pos_.size() == 1u);
      ASSERT(args.named_.empty());

      // TODO check for validity of call
      if (args.pos_[0]->type->is<type::Variant>()) {
        for (auto *t : args.pos_[0]->type->as<type::Variant>().variants_) {
          FnArgs<const type::Type *> call_arg_types;
          call_arg_types.pos_.push_back(args.pos_[0]->type);
          Binding binding;
          binding.fn_expr_ = fn_option;
          binding.exprs_.emplace_back(t, args.pos_[0]);
          table.insert(std::move(call_arg_types), std::move(binding), 1);
        }
      } else {
        FnArgs<const type::Type *> call_arg_types;
        call_arg_types.pos_.push_back(args.pos_[0]->type);
        Binding binding;
        binding.fn_expr_ = fn_option;
        binding.exprs_.emplace_back(args.pos_[0]->type, args.pos_[0]);
        table.insert(std::move(call_arg_types), std::move(binding));
      }

    } else if (fn_option->type == type::Err) {
      // If there's a type error, do I want to exit entirely or assume this
      // one doesn't exist? and just goto next_option?
      return std::nullopt;

    } else {
      UNREACHABLE(fn_option);
    }
  next_option:;
  }
  return std::move(table);
}

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
  STARTING_CHECK_EXPR;
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

void Terminal::VerifyType(Context *) {}
void Hole::VerifyType(Context *) {}
void CodeBlock::VerifyType(Context *) {}

void Identifier::VerifyType(Context *ctx) {
  STARTING_CHECK_EXPR;

  if (decl == nullptr) {
    auto[potential_decls, potential_error_decls] =
        scope_->AllDeclsWithId(token, ctx);

    if (potential_decls.size() != 1) {
      if (potential_decls.empty()) {
        switch (potential_error_decls.size()) {
          case 0: ctx->error_log_.UndeclaredIdentifier(this); break;
          case 1:
            decl = potential_error_decls[0];
            HANDLE_CYCLIC_DEPENDENCIES;
            break;
          default: NOT_YET();
        }
      } else {
        // TODO is this reachable? Or does shadowing cover this case?
        ErrorLog::LogGeneric(
            this->span, "TODO " __FILE__ ":" + std::to_string(__LINE__) + ": ");
      }
      type = type::Err;
      limit_to(StageRange::Nothing());
      return;
    }

    if (potential_decls[0]->const_ && potential_decls[0]->arg_val != nullptr &&
        potential_decls[0]->arg_val->is<GenericFunctionLiteral>()) {
      if (auto iter = ctx->bound_constants_->find(
              potential_decls[0]->identifier->token);
          iter != ctx->bound_constants_->end()) {
        potential_decls[0]->arg_val = scope_->ContainingFnScope()->fn_lit;
      } else {
        ctx->error_log_.UndeclaredIdentifier(this);
        type = type::Err;
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

  // No guarantee the declaration has been validated yet.
  decl->VerifyType(ctx);
  HANDLE_CYCLIC_DEPENDENCIES;
  type = decl->type;
  lvalue = decl->lvalue == Assign::Const ? Assign::Const : Assign::LVal;
}

static std::vector<Expression *> FunctionOptions(const std::string &token,
                                                 Scope *scope, Context *ctx) {
  std::vector<Expression *> fn_options;
  for (auto *scope_ptr = scope; scope_ptr; scope_ptr = scope_ptr->parent) {
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
      return {};
    }

    auto iter = scope_ptr->decls_.find(token);
    if (iter == scope_ptr->decls_.end()) { continue; }
    for (const auto &decl : iter->second) {
      decl->VerifyType(ctx);
      // TODO HANDLE_CYCLIC_DEPENDENCIES;
      if (decl->type == type::Err) { return {}; }
      fn_options.push_back(decl);
    }
  }
  return fn_options;
}

static const type::Type *SetDispatchTable(const FnArgs<Expression *> &args,
                                          std::vector<Expression *> fn_options,
                                          AST::DispatchTable *dispatch_table,
                                          Context *ctx) {
  if (auto maybe_table =
          ComputeDispatchTable(args, std::move(fn_options), ctx)) {
    *dispatch_table = std::move(maybe_table).value();
    std::vector<std::vector<const type::Type *>> out_types;
    out_types.reserve(dispatch_table->bindings_.size());

    if (dispatch_table->bindings_.size() == 0u) { return type::Err; }

    for (const auto & [ key, val ] : dispatch_table->bindings_) {
      if (val.fn_expr_->type->is<type::Function>()) {
        out_types.push_back(val.fn_expr_->type->as<type::Function>().output);
      } else if (val.fn_expr_->type == type::Type_) {
        out_types.push_back({std::get<const type::Type *>(
            Evaluate(val.fn_expr_, ctx)[0].value)});
      } else {
        UNREACHABLE(val.fn_expr_->type);
      }
    }

    ASSERT(!out_types.empty());
    // TODO Can I assume all the lengths are the same?
    std::vector<const type::Type *> var_outs;
    var_outs.reserve(out_types[0].size());
    for (size_t i = 0; i < out_types[0].size(); ++i) {
      std::vector<const type::Type *> types;
      types.reserve(out_types.size());
      for (const auto &out_type : out_types) { types.push_back(out_type[i]); }
      var_outs.push_back(type::Var(types));
    }

    return type::Tup(var_outs);
  } else {
    LOG << "FAIL!"; /* TODO do I need to log an error here? */
    return type::Err;
  }
}

void Binop::VerifyType(Context *ctx) {
  STARTING_CHECK_EXPR;

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

void Call::VerifyType(Context *ctx) {
  STARTING_CHECK_EXPR;
  bool all_const = true;
  args_.Apply([ctx, &all_const, this](auto &arg) {
    arg->VerifyType(ctx);
    HANDLE_CYCLIC_DEPENDENCIES;  // TODO audit macro in lambda
    if (arg->type == type::Err) { this->type = type::Err; }
    all_const &= arg->lvalue == Assign::Const;
  });

  lvalue = all_const ? Assign::Const : Assign::RVal;
  if (type == type::Err) {
    limit_to(StageRange::Nothing());
    return;
  }

  if (fn_->is<Terminal>() && fn_->type != type::Type_) {
    // Special case for error/ord/ascii
    if (fn_->as<Terminal>().value == OrdFunc()) {
      NOT_YET();
    } else if (fn_->as<Terminal>().value == AsciiFunc()) {
      NOT_YET();
    } else if (fn_->as<Terminal>().value == ErrorFunc()) {
      NOT_YET();
    } else {
      UNREACHABLE();
    }
  }

  std::vector<Expression *> fn_options;
  if (!fn_->is<Identifier>()) {
    fn_->VerifyType(ctx);
    HANDLE_CYCLIC_DEPENDENCIES;
    fn_options.push_back(fn_.get());
  } else {
    fn_options = FunctionOptions(fn_->as<Identifier>().token, scope_, ctx);
  }

  if (fn_options.empty()) {
    type = type::Err;
    limit_to(StageRange::Nothing());
    return;
  }

  FnArgs<Expression *> args =
      args_.Transform([](const std::unique_ptr<Expression> &arg) {
        return const_cast<Expression *>(arg.get());
      });

  type = SetDispatchTable(args, std::move(fn_options), &dispatch_table_, ctx);
  if (type == type::Err) { limit_to(StageRange::Nothing()); }

  u64 expanded_size = 1;
  args_.Apply([&expanded_size](auto &arg) {
    if (arg->type->template is<type::Variant>()) {
      expanded_size *= arg->type->template as<type::Variant>().size();
    }
  });

  if (dispatch_table_.total_size_ != expanded_size) {
    LOG << "Failed to find a match for everything. ("
        << dispatch_table_.total_size_ << " vs " << expanded_size << ")";
    type = fn_->type = type::Err;
    limit_to(StageRange::Nothing());
    return;
  }

  if (fn_->is<Identifier>()) {
    // fn_'s type should never be considered beacuse it could be one of many
    // different things. 'type::Void' just indicates that it has been computed
    // (i.e., not 0x0) and that there was no error in doing so (i.e., not
    // type::Err).
    fn_->type = type::Void;
  }
}

void Declaration::VerifyType(Context *ctx) {
  {
    STARTING_CHECK_EXPR;

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

    if (init_val && !init_val->is<Hole>()) {
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

void InDecl::VerifyType(Context *ctx) {
  STARTING_CHECK_EXPR;
  container->VerifyType(ctx);
  HANDLE_CYCLIC_DEPENDENCIES;
  limit_to(container);

  lvalue = Assign::RVal;

  if (container->type == type::Void) {
    type = type::Err;
    identifier->type = type::Err;
    ctx->error_log_.TypeIteration(span);
    limit_to(StageRange::Nothing());
    return;
  }

  if (container->type->is<type::Array>()) {
    type = container->type->as<type::Array>().data_type;

  } else if (container->type == type::Type_) {
    auto t =
        std::get<const type::Type *>(Evaluate(container.get(), ctx)[0].value);
    if (t->is<type::Enum>()) { type = t; }

  } else {
    ctx->error_log_.IndeterminantType(span);
    type = type::Err;
    limit_to(StageRange::Nothing());
  }

  identifier->type = type;
  identifier->VerifyType(ctx);
  HANDLE_CYCLIC_DEPENDENCIES;
}

void Statements::VerifyType(Context *ctx) {
  STARTING_CHECK;
  for (auto &stmt : content_) {
    stmt->VerifyType(ctx);
    limit_to(stmt);
  }
}

void Import::VerifyType(Context *ctx) {
  VERIFY_AND_RETURN_ON_ERROR(operand_);
  lvalue = Assign::Const;
  type = type::Module;
  if (operand_->type != type::String || operand_->lvalue != Assign::Const) {
    ctx->error_log_.InvalidImport(operand_->span);
  } else {
    cache_ = Source::Name{
        std::get<std::string>(Evaluate(operand_.get(), ctx)[0].value)};
    ScheduleModule(*cache_);
  }
  limit_to(operand_);
}

void Unop::VerifyType(Context *ctx) {
  STARTING_CHECK_EXPR;
  VERIFY_AND_RETURN_ON_ERROR(operand);

  using Language::Operator;

  if (op != Operator::At && op != Operator::And) {
    lvalue = operand->lvalue == Assign::Const ? Assign::Const : Assign::LVal;
  }

  switch (op) {
    case Operator::TypeOf:
      type = type::Type_;
      lvalue = Assign::Const;
      break;
    case Operator::Eval: type = operand->type; break;
    case Operator::Generate: type = type::Void; break;
    case Operator::Free: {
      if (!operand->type->is<type::Pointer>()) {
        ctx->error_log_.FreeingNonPointer(operand->type, span);
        limit_to(StageRange::NoEmitIR());
      }
      type = type::Void;
    } break;
    case Operator::Print: {
      if (operand->type == type::Void) {
        ctx->error_log_.PrintingVoid(span);
        limit_to(StageRange::NoEmitIR());
      }
      type = type::Void;
    } break;
    case Operator::Return: {
      if (operand->type == type::Void) {
        // TODO this should probably be allowed (c.f., regular void)
        ctx->error_log_.ReturningVoid(span);
        limit_to(StageRange::NoEmitIR());
      }
      type = type::Void;
    } break;
    case Operator::At: {
      lvalue = Assign::LVal;
      if (operand->type->is<type::Pointer>()) {
        type = operand->type->as<type::Pointer>().pointee;

      } else {
        ctx->error_log_.DereferencingNonPointer(operand->type, span);
        type = type::Err;
        limit_to(StageRange::Nothing());
      }
    } break;
    case Operator::And: {
      switch (operand->lvalue) {
        case Assign::Const:
          ctx->error_log_.TakingAddressOfConstant(span);
          lvalue = Assign::RVal;
          break;
        case Assign::RVal:
          ctx->error_log_.TakingAddressOfTemporary(span);
          lvalue = Assign::RVal;
          break;
        case Assign::LVal: break;
        case Assign::Unset: UNREACHABLE();
      }
      type = Ptr(operand->type);
    } break;
    case Operator::Mul: {
      limit_to(operand);
      if (operand->type != type::Type_) {
        ErrorLog::LogGeneric(
            this->span, "TODO " __FILE__ ":" + std::to_string(__LINE__) + ": ");
        type = type::Err;
        limit_to(StageRange::Nothing());
      } else {
        type = type::Type_;
      }
    } break;
    case Operator::Sub: {
      if (operand->type == type::Int || operand->type == type::Real) {
        type = operand->type;

      } else if (operand->type->is<type::Struct>()) {
        FnArgs<Expression *> args;
        args.pos_  = std::vector{operand.get()};
        type       = SetDispatchTable(args, FunctionOptions("-", scope_, ctx),
                                &dispatch_table_, ctx);
        ASSERT(type, Not(Is<type::Tuple>()));
        if (type == type::Err) { limit_to(StageRange::Nothing()); }
      }
    } break;
    case Operator::Dots: NOT_YET();
    case Operator::Not: {
      if (operand->type == type::Bool) {
        type = type::Bool;
      } else if (operand->type->is<type::Struct>()) {
        FnArgs<Expression *> args;
        args.pos_ = std::vector{operand.get()};
        type      = SetDispatchTable(args, FunctionOptions("!", scope_, ctx),
                                &dispatch_table_, ctx);
        ASSERT(type, Not(Is<type::Tuple>()));
        if (type == type::Err) { limit_to(StageRange::Nothing()); }
      } else {
        ErrorLog::LogGeneric(
            TextSpan(), "TODO " __FILE__ ":" + std::to_string(__LINE__) + ": ");

        type = type::Err;
        limit_to(StageRange::Nothing());
      }
    } break;
    case Operator::Needs: {
      type = type::Void;
      if (operand->type != type::Bool) {
        ctx->error_log_.PreconditionNeedsBool(this);
        limit_to(StageRange::NoEmitIR());
      }
    } break;
    case Operator::Ensure: {
      type = type::Void;
      if (operand->type != type::Bool) {
        ctx->error_log_.PostconditionNeedsBool(this);
        limit_to(StageRange::NoEmitIR());
      }
    } break;
    case Operator::Pass: type = operand->type; break;
    default: UNREACHABLE(*this);
  }
  limit_to(operand);
}

void Access::VerifyType(Context *ctx) {
  STARTING_CHECK_EXPR;
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
  STARTING_CHECK_EXPR;
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
  STARTING_CHECK_EXPR;
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

void ArrayLiteral::VerifyType(Context *ctx) {
  STARTING_CHECK_EXPR;

  lvalue = Assign::Const;
  if (elems.empty()) {
    type = type::EmptyArray;
    return;
  }

  for (auto &elem : elems) {
    elem->VerifyType(ctx);
    HANDLE_CYCLIC_DEPENDENCIES;
    limit_to(elem);
    if (elem->lvalue != Assign::Const) { lvalue = Assign::RVal; }
  }

  const type::Type *joined = type::Err;
  for (auto &elem : elems) {
    joined = type::Join(joined, elem->type);
    if (joined == nullptr) { break; }
  }

  if (joined == nullptr) {
    // type::Types couldn't be joined. Emit an error
    ctx->error_log_.InconsistentArrayType(span);
    type = type::Err;
    limit_to(StageRange::Nothing());
  } else if (joined == type::Err) {
    type = type::Err;  // There were no valid types anywhere in the array
    limit_to(StageRange::Nothing());
  } else {
    type = Arr(joined, elems.size());
  }
}

void ArrayType::VerifyType(Context *ctx) {
  STARTING_CHECK_EXPR;
  lvalue = Assign::Const;

  length->VerifyType(ctx);
  HANDLE_CYCLIC_DEPENDENCIES;
  limit_to(length);
  data_type->VerifyType(ctx);
  HANDLE_CYCLIC_DEPENDENCIES;
  limit_to(data_type);

  type = type::Type_;

  if (length->is<Hole>()) { return; }
  if (length->lvalue != Assign::Const || data_type->lvalue != Assign::Const) {
    lvalue = Assign::RVal;
  }

  if (length->type != type::Int) {
    ctx->error_log_.ArrayIndexType(span);
    limit_to(StageRange::NoEmitIR());
  }
}

void FunctionLiteral::VerifyType(Context *ctx) {
  STARTING_CHECK_EXPR;
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

void Jump::VerifyType(Context *ctx) { STARTING_CHECK; }

void ScopeNode::VerifyType(Context *ctx) {
  STARTING_CHECK_EXPR;

  lvalue = Assign::RVal;

  scope_expr->VerifyType(ctx);
  limit_to(scope_expr);
  if (expr != nullptr) {
    expr->VerifyType(ctx);
    limit_to(expr);
  }
  stmts->VerifyType(ctx);
  limit_to(stmts);

  if (!scope_expr->type->is<type::Scope>()) {
    ErrorLog::InvalidScope(scope_expr->span, scope_expr->type);
    type = type::Err;
    limit_to(StageRange::Nothing());
    return;
  }

  // TODO verify it uses the fields correctly
  type = type::Void;  // TODO can this evaluate to anything?
}

void ScopeLiteral::VerifyType(Context *ctx) {
  STARTING_CHECK_EXPR;
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
  STARTING_CHECK_EXPR;
  lvalue = Assign::Const;
  type = type::Type_;
}
}  // namespace AST

#undef VERIFY_AND_RETURN_ON_ERROR
#undef STARTING_CHECK
#undef STARTING_CHECK_EXPR
