#include "ast/call.h"

#include <sstream>
#include "ast/function_literal.h"
#include "ast/terminal.h"
#include "ast/verify_macros.h"
#include "ir/func.h"
#include "scope.h"
#include "type/function.h"
#include "type/pointer.h"
#include "type/tuple.h"
#include "type/variant.h"

using base::check::Is;

namespace type {
extern Type *String;
extern Type *Code;
extern Type *Int;
extern Type *Char;
}  // namespace type

IR::Val PtrCallFix(const IR::Val& v);
std::vector<IR::Val> Evaluate(AST::Expression *expr, Context *ctx);

IR::Val ErrorFunc() {
  static IR::Func *error_func_ = []() {
    auto fn = new IR::Func(nullptr, type::Func({type::String}, {type::Code}),
                           {{"", nullptr}});
    CURRENT_FUNC(fn) {
      IR::BasicBlock::Current = fn->entry();
      // TODO
      IR::SetReturn(IR::ReturnValue{0}, IR::Err(fn->Argument(0)));
      IR::ReturnJump();
    }
    return fn;
  }();
  return IR::Val::Func(error_func_);
}

IR::Val AsciiFunc() {
  static IR::Func *ascii_func_ = []() {
    auto fn = new IR::Func(nullptr, type::Func({type::Int}, {type::Char}),
                           {{"", nullptr}});
    CURRENT_FUNC(fn) {
      IR::BasicBlock::Current = fn->entry();
      IR::SetReturn(IR::ReturnValue{0}, IR::Trunc(fn->Argument(0)));
      IR::ReturnJump();
    }
    return fn;
  }();
  return IR::Val::Func(ascii_func_);
}

IR::Val OrdFunc() {
  static IR::Func *ord_func_ = []() {
    auto fn = new IR::Func(nullptr, type::Func({type::Char}, {type::Int}),
                           {{"", nullptr}});
    CURRENT_FUNC(fn) {
      IR::BasicBlock::Current = fn->entry();
      IR::SetReturn(IR::ReturnValue{0}, IR::Extend(fn->Argument(0)));
      IR::ReturnJump();
    }
    return fn;
  }();
  return IR::Val::Func(ord_func_);
}

static IR::Val EmitVariantMatch(const IR::Val &needle,
                                const type::Type *haystack) {
  auto runtime_type = IR::Load(IR::VariantType(needle));

  if (haystack->is<type::Variant>()) {
    // TODO I'm fairly confident this will work, but it's also overkill because
    // we may already know this type matches if one variant is a subset of the
    // other.
    auto landing = IR::Func::Current->AddBlock();

    std::vector<IR::Val> phi_args;
    phi_args.reserve(2 * haystack->as<type::Variant>().size() + 2);
    for (const type::Type *v : haystack->as<type::Variant>().variants_) {
      phi_args.push_back(IR::Val::BasicBlock(IR::BasicBlock::Current));
      phi_args.push_back(IR::Val::Bool(true));

      IR::BasicBlock::Current = IR::EarlyExitOn<true>(
          landing, IR::Eq(IR::Val::Type(v), runtime_type));
    }

    phi_args.push_back(IR::Val::BasicBlock(IR::BasicBlock::Current));
    phi_args.push_back(IR::Val::Bool(false));
    IR::UncondJump(landing);

    IR::BasicBlock::Current = landing;
    auto phi = IR::Phi(type::Bool);
    IR::Func::Current->SetArgs(phi, std::move(phi_args));

    return IR::Func::Current->Command(phi).reg();

  } else {
    // TODO actually just implicitly convertible to haystack
    return IR::Eq(IR::Val::Type(haystack), runtime_type);
  }
}

static IR::BlockIndex CallLookupTest(
    const AST::FnArgs<std::pair<AST::Expression *, IR::Val>> &args,
    const AST::FnArgs<const type::Type *> &call_arg_type) {
  // Generate code that attempts to match the types on each argument (only
  // check the ones at the call-site that could be variants).
  auto next_binding = IR::Func::Current->AddBlock();
  for (size_t i = 0; i < args.pos_.size(); ++i) {
    if (!args.pos_[i].first->type->is<type::Variant>()) { continue; }
    IR::BasicBlock::Current = IR::EarlyExitOn<false>(
        next_binding,
        EmitVariantMatch(args.pos_.at(i).second, call_arg_type.pos_[i]));
  }

  for (const auto & [ name, expr_and_val ] : args.named_) {
    auto iter = call_arg_type.find(name);
    if (iter == call_arg_type.named_.end()) { continue; }
    if (!expr_and_val.first->type->is<type::Variant>()) { continue; }
    IR::BasicBlock::Current = IR::EarlyExitOn<false>(
        next_binding,
        EmitVariantMatch(args.named_.at(iter->first).second, iter->second));
  }

  return next_binding;
}

static std::vector<IR::Val> EmitOneCallDispatch(
    const type::Type *ret_type,
    const std::unordered_map<AST::Expression *, const IR::Val *> &expr_map,
    const AST::Binding &binding, Context *ctx) {
  auto callee = binding.fn_expr_->EmitIR(ctx).value;
  if (const type::Type **cast_to = std::get_if<const type::Type *>(&callee)) {
    ASSERT(binding.exprs_.size() == 1u);
    auto[bound_type, expr] = binding.exprs_[0];
    return {IR::Cast(*cast_to, bound_type->PrepareArgument(
                                   expr->type, *expr_map.at(expr), ctx))};
  }

  // After the last check, if you pass, you should dispatch
  IR::Func *fn_to_call = std::visit(
      base::overloaded{[](IR::Func *fn) { return fn; },
                       [](AST::FunctionLiteral *fn) { return fn->ir_func_; },
                       [](auto &&) -> IR::Func * {
                         UNREACHABLE();
                         return nullptr;
                       }},
      callee);
  std::vector<IR::Val> args;
  args.resize(binding.exprs_.size());
  for (size_t i = 0; i < args.size(); ++i) {
    auto[bound_type, expr] = binding.exprs_[i];
    if (expr == nullptr) {
      ASSERT(bound_type != nullptr);
      auto default_expr = fn_to_call->args_[i].second;
      args[i]           = bound_type->PrepareArgument(default_expr->type,
                                            default_expr->EmitIR(ctx), ctx);
    } else {
      args[i] =
          bound_type->PrepareArgument(expr->type, *expr_map.at(expr), ctx);
    }
  }

  ASSERT(fn_to_call != nullptr);

  switch (fn_to_call->type_->output.size()) {
    case 0:
      return std::vector{
          IR::Call(IR::Val::Func(fn_to_call), std::move(args), {})};
    case 1: {
      if (fn_to_call->type_->output AT(0)->is_big()) {
        auto ret_val = IR::Alloca(ret_type);

        if (ret_type->is<type::Variant>()) {
          IR::Call(IR::Val::Func(fn_to_call), std::move(args),
                   std::vector{IR::VariantValue(fn_to_call->type_->output AT(0),
                                                ret_val)});
          IR::Store(IR::Val::Type(fn_to_call->type_->output AT(0)),
                    IR::VariantType(ret_val));
        } else {
          IR::Call(IR::Val::Func(fn_to_call), std::move(args),
                   std::vector{ret_val});
        }
        return {ret_val};
      } else {
        auto ret_val = IR::Alloca(ret_type);
        if (ret_type->is<type::Variant>()) {
          IR::Store(IR::Val::Type(fn_to_call->type_->output AT(0)),
                    IR::VariantType(ret_val));
          IR::Store(IR::Call(IR::Val::Func(fn_to_call), std::move(args), {}),
                    IR::VariantValue(fn_to_call->type_->output AT(0), ret_val));
          return {ret_val};
        } else {
          return {IR::Call(IR::Val::Func(fn_to_call), std::move(args), {})};
        }
      }
    } break;
    default: {
      ASSERT(ret_type, Is<type::Tuple>());
      const auto &tup_entries = ret_type->as<type::Tuple>().entries_;
      ASSERT(fn_to_call->type_->output.size() == tup_entries.size());

      std::vector<IR::Val> ret_vals;
      ret_vals.reserve(fn_to_call->type_->output.size());
      for (auto *entry : tup_entries) { ret_vals.push_back(IR::Alloca(entry)); }

      std::vector<IR::Val> return_args;
      for (size_t i = 0; i < tup_entries.size(); ++i) {
        auto *return_type   = fn_to_call->type_->output AT(i);
        auto *expected_type = tup_entries AT(i);

        if (return_type->is<type::Variant>()) {
          return_args.push_back(ret_vals AT(i));
        } else {
          if (expected_type->is<type::Variant>()) {
            IR::Store(IR::Val::Type(return_type),
                      IR::VariantType(ret_vals AT(i)));
            return_args.push_back(
                IR::VariantValue(return_type, ret_vals AT(i)));
          } else {
            return_args.push_back(ret_vals AT(i));
          }
        }
      }
      IR::Call(IR::Val::Func(fn_to_call), std::move(args), return_args);
      return ret_vals;
    }
  }
  UNREACHABLE();
}

std::vector<IR::Val> EmitCallDispatch(
    const AST::FnArgs<std::pair<AST::Expression *, IR::Val>> &args,
    const AST::DispatchTable &dispatch_table, const type::Type *ret_type,
    Context *ctx) {
  std::unordered_map<AST::Expression *, const IR::Val *> expr_map;
  args.Apply([&expr_map](const std::pair<AST::Expression *, IR::Val> &arg) {
    expr_map[arg.first] = &arg.second;
  });

  if (dispatch_table.bindings_.size() == 1) {
    const auto & [ call_arg_type, binding ] = *dispatch_table.bindings_.begin();
    return EmitOneCallDispatch(ret_type, expr_map, binding, ctx);
  }

  size_t num_rets = ret_type->is<type::Tuple>()
                        ? ret_type->as<type::Tuple>().entries_.size()
                        : 1;

  std::vector<std::vector<IR::Val>> result_phi_args(num_rets);
  for (auto &result : result_phi_args) {
    result.reserve(2 * dispatch_table.bindings_.size());
  }

  auto landing_block = IR::Func::Current->AddBlock();

  auto iter = dispatch_table.bindings_.begin();
  for (size_t i = 0; i < dispatch_table.bindings_.size() - 1; ++i, ++iter) {
    const auto & [ call_arg_type, binding ] = *iter;
    auto next_binding = CallLookupTest(args, call_arg_type);
    size_t j          = 0;
    for (auto &&result :
         EmitOneCallDispatch(ret_type, expr_map, binding, ctx)) {
      result_phi_args AT(j).push_back(IR::Val::BasicBlock(IR::BasicBlock::Current));
      result_phi_args AT(j).push_back(std::move(result));
      ++j;
    }
    ASSERT(j == num_rets);

    IR::UncondJump(landing_block);
    IR::BasicBlock::Current = next_binding;
  }

  const auto & [ call_arg_type, binding ] = *iter;
  size_t j                                = 0;
  for (auto &&result : EmitOneCallDispatch(ret_type, expr_map, binding, ctx)) {
    result_phi_args AT(j).push_back(IR::Val::BasicBlock(IR::BasicBlock::Current));
    result_phi_args AT(j).push_back(std::move(result));
    ++j;
  }
  ASSERT(j == num_rets);

  IR::UncondJump(landing_block);
  IR::BasicBlock::Current = landing_block;

  switch (num_rets) {
    case 0: return {};
    case 1:
      if (ret_type == type::Void) {
        return std::vector(1, IR::Val::None());
      } else {
        auto phi = IR::Phi(ret_type->is_big() ? Ptr(ret_type) : ret_type);
        IR::Func::Current->SetArgs(phi, std::move(result_phi_args[0]));
        return std::vector(1, IR::Func::Current->Command(phi).reg());
      }
    default: {
      std::vector<IR::Val> results;
      results.reserve(num_rets);
      const auto &tup_entries = ret_type->as<type::Tuple>().entries_;
      for (size_t i = 0; i < num_rets; ++i) {
        const type::Type *single_ret_type = tup_entries[i];
        if (single_ret_type == type::Void) {
          results.push_back(IR::Val::None());
        } else {
          auto phi = IR::Phi(single_ret_type->is_big() ? Ptr(single_ret_type)
                                                       : single_ret_type);
          IR::Func::Current->SetArgs(phi, std::move(result_phi_args[i]));
          results.push_back(IR::Func::Current->Command(phi).reg());
        }
      }
      return results;
    } break;
  }
  UNREACHABLE();
}

namespace AST {

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

std::vector<Expression *> FunctionOptions(const std::string &token,
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

const type::Type *SetDispatchTable(const FnArgs<Expression *> &args,
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

std::string Call::to_string(size_t n) const {
  std::stringstream ss;
  ss << fn_->to_string(n) << "(";
  bool seen_one = false;
  for (const auto &pos : args_.pos_) {
    ss << (seen_one ? ", " : "") << pos->to_string(n);
    seen_one = true;
  }
  for (const auto & [ key, val ] : args_.named_) {
    ss << (seen_one ? ", " : "") << key << " = " << val->to_string(n) << ", ";
    seen_one = true;
  }
  ss << ")";
  return ss.str();
}

void Call::assign_scope(Scope *scope) {
  STAGE_CHECK(AssignScopeStage, AssignScopeStage);
  scope_ = scope;
  fn_->assign_scope(scope);
  args_.Apply([scope](auto &expr) { expr->assign_scope(scope); });
}

void Call::ClearIdDecls() {
  stage_range_ = StageRange{};
  fn_->ClearIdDecls();
  args_.Apply([](auto &expr) { expr->ClearIdDecls(); });
}

void Call::VerifyType(Context *ctx) {
  VERIFY_STARTING_CHECK_EXPR;
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

void Call::Validate(Context *ctx) {
  STAGE_CHECK(StartBodyValidationStage, DoneBodyValidationStage);
  fn_->Validate(ctx);
  args_.Apply([ctx](auto &arg) { arg->Validate(ctx); });
}

void Call::SaveReferences(Scope *scope, std::vector<IR::Val> *args) {
  for (auto &pos : args_.pos_) { pos->SaveReferences(scope, args); }
  for (auto & [ name, expr ] : args_.named_) {
    expr->SaveReferences(scope, args);
  }
}

void Call::contextualize(
    const Node *correspondant,
    const std::unordered_map<const Expression *, IR::Val> &replacements) {
  fn_->contextualize(correspondant->as<Call>().fn_.get(), replacements);

  for (size_t i = 0; i < args_.pos_.size(); ++i) {
    args_.pos_[i]->contextualize(correspondant->as<Call>().args_.pos_[i].get(),
                                 replacements);
  }
  for (auto && [ name, expr ] : args_.named_) {
    expr->contextualize(
        correspondant->as<Call>().args_.named_.find(name)->second.get(),
        replacements);
  }
}

void Call::ExtractReturns(std::vector<const Expression *> *rets) const {
  fn_->ExtractReturns(rets);
  for (const auto &val : args_.pos_) { val->ExtractReturns(rets); }
  for (const auto & [ key, val ] : args_.named_) { val->ExtractReturns(rets); }
}

Call *Call::Clone() const {
  auto *result = new Call;
  result->span = span;
  result->fn_  = base::wrap_unique(fn_->Clone());
  result->args_.pos_.reserve(args_.pos_.size());
  for (const auto &val : args_.pos_) {
    result->args_.pos_.emplace_back(val->Clone());
  }
  for (const auto & [ key, val ] : args_.named_) {
    result->args_.named_.emplace(key, base::wrap_unique(val->Clone()));
  }

  result->dispatch_table_ = dispatch_table_;
  return result;
}

IR::Val AST::Call::EmitIR(Context *ctx) {
  if (fn_->is<Terminal>() && fn_->type != type::Type_) {
    return IR::Call(ErrorFunc(), {args_.pos_[0]->EmitIR(ctx)}, {});
  }

  ASSERT(dispatch_table_.bindings_.size() > 0u);
  // Look at all the possible calls and generate the dispatching code
  // TODO implement this with a lookup table instead of this branching
  // insanity.

  // TODO an opmitimazion we can do is merging all the allocas for results
  // into a single variant buffer, because we know we need something that big
  // anyway, and their use cannot overlap.

  auto results = EmitCallDispatch(
      args_.Transform([ctx](const std::unique_ptr<Expression> &expr) {
        return std::pair(const_cast<Expression *>(expr.get()),
                         expr->type->is_big() ? PtrCallFix(expr->EmitIR(ctx))
                                              : expr->EmitIR(ctx));
      }),
      dispatch_table_, type, ctx);

  switch (results.size()) {
    case 0: return IR::Val::None();
    case 1: return std::move(results)[0];
    default: return IR::Val::Many(results);
  }
}

IR::Val Call::EmitLVal(Context *) { UNREACHABLE(this); }
}  // namespace AST
