#include "func.h"

#include <future>
#include <algorithm>
#include <optional>

#include "../ast/ast.h"
#include "../base/guarded.h"
#include "../context.h"
#include "../error/log.h"
#include "../type/all.h"
#include "../ast/stages.h"

using base::check::Is;

extern base::guarded<std::unordered_map<
    Source::Name, std::shared_future<std::unique_ptr<Module>>>>
    modules;


std::vector<IR::Val> Evaluate(AST::Expression *expr, Context *ctx);
extern std::vector<IR::Val> global_vals;

// If the expression is a CommaList, apply the function to each expr. Otherwise
// call it on the expression itself.
void ForEachExpr(AST::Expression *expr,
                 const std::function<void(size_t, AST::Expression *)> &fn) {
  if (expr->is<AST::CommaList>()) {
    const auto &exprs = expr->as<AST::CommaList>().exprs;
    for (size_t i = 0; i < exprs.size(); ++i) { fn(i, exprs[i].get()); }
  } else {
    fn(0, expr);
  }
}

// TODO using nullptr for module. Is that safe here? Is it correct?

IR::Val ErrorFunc() {
  static IR::Func *error_func_ = []() {
    auto fn = new IR::Func(nullptr, type::Func({type::String}, {type::Code}),
                           {{"", nullptr}});
    CURRENT_FUNC(fn) {
      IR::Block::Current = fn->entry();
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
      IR::Block::Current = fn->entry();
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
      IR::Block::Current = fn->entry();
      IR::SetReturn(IR::ReturnValue{0}, IR::Extend(fn->Argument(0)));
      IR::ReturnJump();
    }
    return fn;
  }();
  return IR::Val::Func(ord_func_);
}

IR::Val AST::Access::EmitLVal(Context *ctx) {
  auto val = operand->EmitLVal(ctx);
  while (val.type->is<type::Pointer>() &&
         !val.type->as<type::Pointer>().pointee->is_big()) {
    val = IR::Load(val);
  }

  ASSERT(val.type, Is<type::Pointer>());
  ASSERT(val.type->as<type::Pointer>().pointee, Is<type::Struct>());

  auto *struct_type =
      &val.type->as<type::Pointer>().pointee->as<type::Struct>();
  return IR::Field(val, struct_type->field_indices_.at(member_name));
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
      phi_args.push_back(IR::Val::Block(IR::Block::Current));
      phi_args.push_back(IR::Val::Bool(true));

      IR::Block::Current = IR::EarlyExitOn<true>(
          landing, IR::Eq(IR::Val::Type(v), runtime_type));
    }

    phi_args.push_back(IR::Val::Block(IR::Block::Current));
    phi_args.push_back(IR::Val::Bool(false));
    IR::UncondJump(landing);

    IR::Block::Current = landing;
    auto phi = IR::Phi(type::Bool);
    IR::Func::Current->SetArgs(phi, std::move(phi_args));

    return IR::Func::Current->Command(phi).reg();

  } else {
    // TODO actually just implicitly convertible to haystack
    return IR::Eq(IR::Val::Type(haystack), runtime_type);
  }
}

IR::Val AST::Node::EmitIR(Context *) { UNREACHABLE(*this); }
IR::Val AST::Expression::EmitIR(Context *) { UNREACHABLE(*this); }
IR::Val AST::Expression::EmitLVal(Context *) { UNREACHABLE(*this); }

IR::Val AST::Jump::EmitIR(Context *) {
  switch (jump_type) {
    case JumpType::Return: IR::ReturnJump(); return IR::Val::None();
    case JumpType::Restart: NOT_YET();
    case JumpType::Repeat: NOT_YET();
    case JumpType::Break: NOT_YET();
    case JumpType::Continue: NOT_YET();
  }
  UNREACHABLE();
}

static IR::BlockIndex CallLookupTest(
    const AST::FnArgs<std::pair<AST::Expression *, IR::Val>> &args,
    const AST::FnArgs<const type::Type *> &call_arg_type) {
  // Generate code that attempts to match the types on each argument (only
  // check the ones at the call-site that could be variants).
  auto next_binding = IR::Func::Current->AddBlock();
  for (size_t i = 0; i < args.pos_.size(); ++i) {
    if (!args.pos_[i].first->type->is<type::Variant>()) { continue; }
    IR::Block::Current = IR::EarlyExitOn<false>(
        next_binding,
        EmitVariantMatch(args.pos_.at(i).second, call_arg_type.pos_[i]));
  }

  for (const auto & [ name, expr_and_val ] : args.named_) {
    auto iter = call_arg_type.find(name);
    if (iter == call_arg_type.named_.end()) { continue; }
    if (!expr_and_val.first->type->is<type::Variant>()) { continue; }
    IR::Block::Current = IR::EarlyExitOn<false>(
        next_binding,
        EmitVariantMatch(args.named_.at(iter->first).second, iter->second));
  }

  return next_binding;
}

static IR::Val EmitOneCallDispatch(
    const type::Type *ret_type,
    const std::unordered_map<AST::Expression *, const IR::Val *> &expr_map,
    const AST::Binding &binding, Context *ctx) {
  auto callee = binding.fn_expr_->EmitIR(ctx).value;
  if (const type::Type **cast_to = std::get_if<const type::Type *>(&callee)) {
    ASSERT(binding.exprs_.size() == 1u);
    auto[bound_type, expr] = binding.exprs_[0];
    return IR::Cast(*cast_to, bound_type->PrepareArgument(
                                  expr->type, *expr_map.at(expr), ctx));
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

  if (fn_to_call->type_->output.size() > 1) {
    NOT_YET(fn_to_call->type_->output);
  }
  IR::Val ret_val;
  auto *out_type = fn_to_call->type_->output.empty()
                       ? type::Void
                       : fn_to_call->type_->output AT(0);
  if (out_type->is_big()) {
    ret_val = IR::Alloca(ret_type);
    IR::Call(IR::Val::Func(fn_to_call), std::move(args), {ret_val});
  } else {
    if (ret_type->is_big()) {
      ret_val = IR::Alloca(ret_type);
      ret_type->EmitAssign(
          out_type, IR::Call(IR::Val::Func(fn_to_call), std::move(args), {}),
          ret_val, ctx);
    } else {
      ret_val = IR::Call(IR::Val::Func(fn_to_call), std::move(args), {});
    }
  }

  return ret_val;
}

static IR::Val EmitCallDispatch(
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

  std::vector<IR::Val> results;
  results.reserve(2 * dispatch_table.bindings_.size());

  auto landing_block = IR::Func::Current->AddBlock();

  auto iter = dispatch_table.bindings_.begin();
  for (size_t i = 0; i < dispatch_table.bindings_.size() - 1; ++i, ++iter) {
    const auto & [ call_arg_type, binding ] = *iter;
    auto next_binding = CallLookupTest(args, call_arg_type);
    auto result = EmitOneCallDispatch(ret_type, expr_map, binding, ctx);
    results.push_back(IR::Val::Block(IR::Block::Current));
    results.push_back(std::move(result));

    IR::UncondJump(landing_block);
    IR::Block::Current = next_binding;
  }

  const auto & [ call_arg_type, binding ] = *iter;
  results.push_back(IR::Val::Block(IR::Block::Current));
  results.push_back(EmitOneCallDispatch(ret_type, expr_map, binding, ctx));

  IR::UncondJump(landing_block);
  IR::Block::Current = landing_block;

  if (results.empty()) {
    return IR::Val::None();
  } else {
    if (results.size() == 2) {
      return results[1];
    } else if (ret_type != type::Void) {
      auto phi = IR::Phi(ret_type->is_big() ? Ptr(ret_type) : ret_type);
      IR::Func::Current->SetArgs(phi, std::move(results));
      return IR::Func::Current->Command(phi).reg();
    } else {
      return IR::Val::None();
    }
  }
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

  return EmitCallDispatch(
      args_.Transform([ctx](const std::unique_ptr<Expression> &expr) {
        return std::pair(const_cast<Expression *>(expr.get()),
                         expr->type->is_big() ? PtrCallFix(expr->EmitIR(ctx))
                                              : expr->EmitIR(ctx));
      }),
      dispatch_table_, type, ctx);
}

IR::Val AST::Access::EmitIR(Context *ctx) {
  if (operand->type == type::Module) {
    auto mod =
        std::get<const Module *>(Evaluate(operand.get(), ctx) AT(0).value);
    return mod->GetDecl(member_name)->EmitIR(ctx);
  } else if (type->is<type::Enum>()) {
    return type->as<type::Enum>().EmitLiteral(member_name);
  } else {
    return PtrCallFix(EmitLVal(ctx));
  }
  return IR::Val::None();
}

IR::Val AST::Terminal::EmitIR(Context *) { return value; }

IR::Val AST::Identifier::EmitIR(Context *ctx) {
  auto *val = AST::find(ctx->bound_constants_, token);
  if (!decl) { return val ? *val : IR::Val::None(); }

  if (decl->const_) {
    if (val) { return *val; }

    if (decl->IsCustomInitialized()) {
      return Evaluate(decl->init_val.get(), ctx) AT(0);

    } else {
      // TODO this may be wrong for types that require nontrivial construction.
      return decl->type->EmitInitialValue(ctx);
    }
  }

  // TODO this global scope thing is probably wrong.
  if (decl->scope_ == ctx->mod_->global_.get()) { decl->EmitIR(ctx); }

  if (decl->arg_val) {
    return decl->addr;
  } else if (decl->is<InDecl>()) {
    if (auto &in_decl = decl->as<InDecl>();
        in_decl.container->type->is<type::Array>()) {
      return PtrCallFix(EmitLVal(ctx));
    } else {
      return decl->addr;
    }
  } else {
    return PtrCallFix(EmitLVal(ctx));
  }
}

IR::Val AST::ArrayLiteral::EmitIR(Context *ctx) {
  auto array_val = IR::Alloca(type);
  auto *data_type = type->as<type::Array>().data_type;
  for (size_t i = 0; i < elems.size(); ++i) {
    auto elem_i = IR::Index(array_val, IR::Val::Int(static_cast<i32>(i)));
    type::EmitMoveInit(data_type, data_type, elems[i]->EmitIR(ctx), elem_i,
                       ctx);
  }
  return array_val;
}

IR::Val AST::For::EmitIR(Context *ctx) {
  auto init = IR::Func::Current->AddBlock();
  auto incr = IR::Func::Current->AddBlock();
  auto phi = IR::Func::Current->AddBlock();
  auto cond = IR::Func::Current->AddBlock();
  auto body_entry = IR::Func::Current->AddBlock();
  auto exit = IR::Func::Current->AddBlock();

  IR::UncondJump(init);

  std::vector<IR::Val> init_vals;
  init_vals.reserve(iterators.size());
  {  // Init block
    IR::Block::Current = init;
    for (auto &decl : iterators) {
      if (decl->container->type->is<type::Range>()) {
        if (decl->container->is<Binop>()) {
          init_vals.push_back(decl->container->as<Binop>().lhs->EmitIR(ctx));
        } else if (decl->container->is<Unop>()) {
          init_vals.push_back(decl->container->as<Unop>().operand->EmitIR(ctx));
        } else {
          NOT_YET();
        }
      } else if (decl->container->type->is<type::Array>()) {
        init_vals.push_back(
            IR::Index(decl->container->EmitLVal(ctx), IR::Val::Int(0)));

      } else if (decl->container->type == type::Type_) {
        // TODO this conditional check on the line above is wrong
        IR::Val container_val = Evaluate(decl->container.get(), ctx)[0];
        if (const type::Type *t =
                std::get<const type::Type *>(container_val.value);
            t->is<type::Enum>()) {
          init_vals.push_back(t->as<type::Enum>().EmitInitialValue(ctx));
        } else {
          NOT_YET();
        }
      } else {
        NOT_YET();
      }
    }
    IR::UncondJump(phi);
  }

  std::vector<IR::CmdIndex> phis;
  phis.reserve(iterators.size());
  {  // Phi block
    IR::Block::Current = phi;
    for (auto &decl : iterators) {
      if (decl->container->type == type::Type_) {
        IR::Val container_val = Evaluate(decl->container.get(), ctx)[0];
        if (auto *t = std::get<const type::Type *>(container_val.value);
            t->is<type::Enum>()) {
          phis.push_back(IR::Phi(&t->as<type::Enum>()));
        } else {
          NOT_YET();
        }
      } else if (decl->container->type->is<type::Range>()) {
        phis.push_back(
            IR::Phi(decl->container->type->as<type::Range>().end_type));
      } else if (decl->container->type->is<type::Array>()) {
        phis.push_back(
            IR::Phi(Ptr(decl->container->type->as<type::Array>().data_type)));
      } else {
        NOT_YET(*decl->container->type);
      }
    }
    IR::UncondJump(cond);
  }

  std::vector<IR::Val> incr_vals;
  incr_vals.reserve(iterators.size());
  {  // Incr block
    IR::Block::Current = incr;
    for (auto iter : phis) {
      auto phi_reg = IR::Func::Current->Command(iter).reg();
      if (phi_reg.type == type::Int) {
        incr_vals.push_back(IR::Add(phi_reg, IR::Val::Int(1)));
      } else if (phi_reg.type == type::Char) {
        incr_vals.push_back(IR::Add(phi_reg, IR::Val::Char(1)));
      } else if (phi_reg.type->is<type::Enum>()) {
        incr_vals.push_back(IR::Add(
            phi_reg, IR::Val::Enum(&phi_reg.type->as<type::Enum>(), 1)));
      } else if (phi_reg.type->is<type::Pointer>()) {
        incr_vals.push_back(IR::PtrIncr(phi_reg, IR::Val::Int(1)));
      } else {
        NOT_YET();
      }
    }
    IR::UncondJump(phi);
  }

  {  // Complete phi definition
    for (size_t i = 0; i < iterators.size(); ++i) {
      IR::Func::Current->SetArgs(phis[i], {IR::Val::Block(init), init_vals[i],
                                           IR::Val::Block(incr), incr_vals[i]});
      iterators[i]->addr = IR::Func::Current->Command(phis[i]).reg();
    }
  }

  {  // Cond block
    IR::Block::Current = cond;
    for (size_t i = 0; i < iterators.size(); ++i) {
      auto *decl = iterators[i].get();
      auto reg = IR::Func::Current->Command(phis[i]).reg();
      auto next = IR::Func::Current->AddBlock();
      IR::Val cmp;
      if (decl->container->type->is<type::Range>()) {
        if (decl->container->is<Binop>()) {
          auto rhs_val = decl->container->as<Binop>().rhs->EmitIR(ctx);
          cmp = IR::Le(reg, rhs_val);
        } else if (decl->container->is<Unop>()) {
          // TODO we should optimize this here rather then generate suboptimal
          // code and trust optimizations later on.
          cmp = IR::Val::Bool(true);
        } else {
          NOT_YET();
        }
      } else if (decl->container->type->is<type::Array>()) {
        auto *array_type = &decl->container->type->as<type::Array>();
        cmp = IR::Ne(
            reg, IR::Index(decl->container->EmitLVal(ctx),
                           IR::Val::Int(static_cast<i32>(array_type->len))));
      } else if (decl->container->type == type::Type_) {
        IR::Val container_val = Evaluate(decl->container.get(), ctx)[0];
        if (auto *t = std::get<const type::Type *>(container_val.value);
            t->is<type::Enum>()) {
          cmp = IR::Le(reg,
                       IR::Val::Enum(&t->as<type::Enum>(),
                                     t->as<type::Enum>().members_.size() - 1));
        } else {
          NOT_YET();
        }
      } else {
        NOT_YET();
      }

      IR::CondJump(cmp, next, exit);
      IR::Block::Current = next;
    }
    IR::UncondJump(body_entry);
  }

  {  // Body
    IR::Block::Current = body_entry;

    for_scope->Enter(ctx);
    statements->EmitIR(ctx);
    for_scope->Exit(ctx);

    IR::UncondJump(incr);
  }

  IR::Block::Current = exit;
  return IR::Val::None();
}

IR::Val AST::ScopeLiteral::EmitIR(Context *ctx) {
  enter_fn->init_val->EmitIR(ctx);
  exit_fn->init_val->EmitIR(ctx);
  return IR::Val::Scope(this);
}

IR::Val AST::ScopeNode::EmitIR(Context *ctx) {
  IR::Val scope_expr_val = Evaluate(scope_expr.get(), ctx)[0];
  ASSERT(scope_expr_val.type, Is<type::Scope>());

  auto enter_fn =
      std::get<ScopeLiteral *>(scope_expr_val.value)->enter_fn->init_val.get();
  enter_fn->EmitIR(ctx);

  auto exit_fn =
      std::get<ScopeLiteral *>(scope_expr_val.value)->exit_fn->init_val.get();
  exit_fn->EmitIR(ctx);

  auto enter_block = IR::Func::Current->AddBlock();
  IR::UncondJump(enter_block);
  IR::Block::Current = enter_block;

  auto call_enter_result = IR::Call(
      IR::Val::Func(enter_fn->as<FunctionLiteral>().ir_func_),
      expr ? std::vector<IR::Val>{expr->EmitIR(ctx)} : std::vector<IR::Val>{},
      {});
  auto land_block = IR::Func::Current->AddBlock();
  auto body_start_block = IR::Func::Current->AddBlock();

  IR::CondJump(call_enter_result, body_start_block, land_block);

  IR::Block::Current = body_start_block;
  stmts->EmitIR(ctx);

  auto call_exit_result =
      IR::Call(IR::Val::Func(exit_fn->as<FunctionLiteral>().ir_func_), {}, {});
  IR::CondJump(call_exit_result, enter_block, land_block);

  IR::Block::Current = land_block;
  return IR::Val::None();
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
      // TODO if EmitInitialValue requires generating code, that would be bad.
      addr = type->EmitInitialValue(ctx);
    } else {
      UNREACHABLE();
    }
  } else if (scope_ == ctx->mod_->global_.get()) {
    // TODO these checks actually overlap and could be simplified.
    if (IsUninitialized()) {
      global_vals.emplace_back();
      global_vals.back().type = type;
      addr = IR::Val::GlobalAddr(global_vals.size() - 1, type);
    } else if (IsCustomInitialized()) {
      auto eval = Evaluate(init_val.get(), ctx);
      if (ctx->num_errors()) { return IR::Val::None(); }
      global_vals.push_back(eval[0]);
      addr = IR::Val::GlobalAddr(global_vals.size() - 1, type);

    } else if (IsDefaultInitialized()) {
      // TODO if EmitInitialValue requires generating code, that would be bad.
      global_vals.push_back(type->EmitInitialValue(ctx));
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
    if (IsUninitialized()) { return IR::Val::None(); }
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

IR::Val AST::Import::EmitIR(Context *ctx) {
  ASSERT(cache_.has_value());
  return IR::Val::Mod(modules.lock()->at(*cache_).get().get());
}

IR::Val AST::Unop::EmitIR(Context *ctx) {
  if (operand->type->is<type::Struct>() && dispatch_table_.total_size_ != 0) {
    // TODO struct is not exactly right. we really mean user-defined
    AST::FnArgs<std::pair<AST::Expression *, IR::Val>> args;
    args.pos_ = {std::pair(operand.get(), operand->type->is_big()
                                              ? PtrCallFix(operand->EmitIR(ctx))
                                              : operand->EmitIR(ctx))};
    return EmitCallDispatch(args, dispatch_table_, type, ctx);
  }

  switch (op) {
    case Language::Operator::Not:
    case Language::Operator::Sub: {
      return IR::Neg(operand->EmitIR(ctx));
    } break;
    case Language::Operator::Return: {
      ForEachExpr(operand.get(), [ctx](size_t i, AST::Expression *expr) {
        IR::SetReturn(IR::ReturnValue{static_cast<i32>(i)}, expr->EmitIR(ctx));
      });
      IR::ReturnJump();
      return IR::Val::None();
    }
    case Language::Operator::TypeOf: return IR::Val::Type(operand->type);
    case Language::Operator::Print: {
      ForEachExpr(
          operand.get(), [&ctx](size_t, AST::Expression *expr) {
            if (expr->type->is<type::Primitive>() ||
                expr->type->is<type::Pointer>()) {
              IR::Print(expr->EmitIR(ctx));
            } else {
              expr->type->EmitRepr(expr->EmitIR(ctx), ctx);
            }
          });

      return IR::Val::None();
    } break;
    case Language::Operator::And: return operand->EmitLVal(ctx);
    case Language::Operator::Eval: {
      // TODO what if there's an error during evaluation?
      // TODO what about ``a, b = $FnWithMultipleReturnValues()``
      auto results = Evaluate(operand.get(), ctx);
      return results.empty() ? IR::Val::None() : results[0];
    }
    case Language::Operator::Generate: {
      auto val = Evaluate(operand.get(), ctx) AT(0);
      ASSERT(val.type == type::Code);
      auto block = std::get<AST::CodeBlock>(val.value);
      if (auto *err = std::get_if<std::string>(&block.content_)) {
        ctx->error_log_.UserDefinedError(*err);
        return IR::Val::None();
      }

      auto *stmts = &std::get<AST::Statements>(block.content_);
      stmts->assign_scope(scope_);
      stmts->VerifyType(ctx);
      stmts->Validate(ctx);
      return stmts->EmitIR(ctx);

    } break;
    case Language::Operator::Mul: return IR::Ptr(operand->EmitIR(ctx));
    case Language::Operator::At: return PtrCallFix(operand->EmitIR(ctx));
    case Language::Operator::Needs: {
      // TODO validate requirements are well-formed?
      IR::Func::Current->preconditions_.push_back(operand.get());
      return IR::Val::None();
    } break;
    case Language::Operator::Ensure: {
      // TODO validate requirements are well-formed?
      IR::Func::Current->postconditions_.push_back(operand.get());
      return IR::Val::None();
    } break;
    case Language::Operator::Pass: return operand->EmitIR(ctx);
    default: UNREACHABLE("Operator is ", static_cast<int>(op));
  }
    }

    IR::Val AST::Binop::EmitIR(Context * ctx) {
      if (lhs->type->is<type::Struct>() || rhs->type->is<type::Struct>()) {
        // TODO struct is not exactly right. we really mean user-defined
        AST::FnArgs<std::pair<AST::Expression *, IR::Val>> args;
        args.pos_.reserve(2);
        args.pos_.emplace_back(lhs.get(), lhs->type->is_big()
                                              ? PtrCallFix(lhs->EmitIR(ctx))
                                              : lhs->EmitIR(ctx));
        args.pos_.emplace_back(rhs.get(), rhs->type->is_big()
                                              ? PtrCallFix(rhs->EmitIR(ctx))
                                              : rhs->EmitIR(ctx));
        return EmitCallDispatch(args, dispatch_table_, type, ctx);
      }

      switch (op) {
#define CASE(op_name)                                                          \
  case Language::Operator::op_name: {                                          \
    auto lhs_ir = lhs->EmitIR(ctx);                                            \
    auto rhs_ir = rhs->EmitIR(ctx);                                            \
    return IR::op_name(lhs_ir, rhs_ir);                                        \
  } break
    CASE(Add);
    CASE(Sub);
    CASE(Mul);
    CASE(Div);
    CASE(Mod);
    CASE(Arrow);
#undef CASE
    case Language::Operator::Assign: {
      std::vector<const type::Type *> lhs_types, rhs_types;
      std::vector<IR::Val> rhs_vals;
      ForEachExpr(rhs.get(),
                  [&ctx, &rhs_vals, &rhs_types](size_t, AST::Expression *expr) {
                    rhs_vals.push_back(expr->EmitIR(ctx));
                    rhs_types.push_back(expr->type);
                  });

      std::vector<IR::Val> lhs_lvals;
      ForEachExpr(lhs.get(), [&ctx, &lhs_lvals, &lhs_types](
                                 size_t, AST::Expression *expr) {
        lhs_lvals.push_back(expr->EmitLVal(ctx));
        lhs_types.push_back(expr->type);
      });

      ASSERT(lhs_lvals.size() == rhs_vals.size());
      for (size_t i = 0; i < lhs_lvals.size(); ++i) {
        lhs_types[i]->EmitAssign(rhs_types[i], PtrCallFix(rhs_vals[i]),
                                 lhs_lvals[i], ctx);
      }
      return IR::Val::None();
    } break;
    case Language::Operator::OrEq: {
      if (type->is<type::Enum>()) {
        auto lhs_lval = lhs->EmitLVal(ctx);
        IR::Store(IR::Or(IR::Load(lhs_lval), rhs->EmitIR(ctx)), lhs_lval);
        return IR::Val::None();
      }
      auto land_block = IR::Func::Current->AddBlock();
      auto more_block = IR::Func::Current->AddBlock();

      auto lhs_val = lhs->EmitIR(ctx);
      auto lhs_end_block = IR::Block::Current;
      IR::CondJump(lhs_val, land_block, more_block);

      IR::Block::Current = more_block;
      auto rhs_val = rhs->EmitIR(ctx);
      auto rhs_end_block = IR::Block::Current;
      IR::UncondJump(land_block);

      IR::Block::Current = land_block;

      auto phi = IR::Phi(type::Bool);
      IR::Func::Current->SetArgs(
          phi, {IR::Val::Block(lhs_end_block), IR::Val::Bool(true),
                IR::Val::Block(rhs_end_block), rhs_val});
      return IR::Func::Current->Command(phi).reg();
    } break;
    case Language::Operator::AndEq: {
      if (type->is<type::Enum>()) {
        auto lhs_lval = lhs->EmitLVal(ctx);
        IR::Store(IR::And(IR::Load(lhs_lval), rhs->EmitIR(ctx)), lhs_lval);
        return IR::Val::None();
      }

      auto land_block = IR::Func::Current->AddBlock();
      auto more_block = IR::Func::Current->AddBlock();

      auto lhs_val = lhs->EmitIR(ctx);
      auto lhs_end_block = IR::Block::Current;
      IR::CondJump(lhs_val, more_block, land_block);

      IR::Block::Current = more_block;
      auto rhs_val = rhs->EmitIR(ctx);
      auto rhs_end_block = IR::Block::Current;
      IR::UncondJump(land_block);

      IR::Block::Current = land_block;

      auto phi = IR::Phi(type::Bool);
      IR::Func::Current->SetArgs(
          phi, {IR::Val::Block(lhs_end_block), IR::Val::Bool(false),
                IR::Val::Block(rhs_end_block), rhs_val});
      return IR::Func::Current->Command(phi).reg();
    } break;
#define CASE_ASSIGN_EQ(op_name)                                                \
  case Language::Operator::op_name##Eq: {                                      \
    auto lhs_lval = lhs->EmitLVal(ctx);                                        \
    auto rhs_ir = rhs->EmitIR(ctx);                                            \
    IR::Store(IR::op_name(PtrCallFix(lhs_lval), rhs_ir), lhs_lval);            \
    return IR::Val::None();                                                    \
  } break
      CASE_ASSIGN_EQ(Xor);
      CASE_ASSIGN_EQ(Add);
      CASE_ASSIGN_EQ(Sub);
      CASE_ASSIGN_EQ(Mul);
      CASE_ASSIGN_EQ(Div);
      CASE_ASSIGN_EQ(Mod);
#undef CASE_ASSIGN_EQ
    case Language::Operator::Index: return PtrCallFix(EmitLVal(ctx));
    default: UNREACHABLE(*this);
  }
}

IR::Val AST::ArrayType::EmitIR(Context *ctx) {
  return IR::Array(length->EmitIR(ctx), data_type->EmitIR(ctx));
}

static IR::Val EmitChainOpPair(AST::ChainOp *chain_op, size_t index,
                               const IR::Val &lhs_ir, const IR::Val &rhs_ir,
                               Context *ctx) {
  const type::Type *lhs_type = chain_op->exprs[index]->type;
  const type::Type *rhs_type = chain_op->exprs[index + 1]->type;
  auto op = chain_op->ops[index];

  if (lhs_type->is<type::Array>() && rhs_type->is<type::Array>()) {
    ASSERT(op == Language::Operator::Eq || op == Language::Operator::Ne);
    return type::Array::Compare(&lhs_type->as<type::Array>(), lhs_ir,
                                &rhs_type->as<type::Array>(), rhs_ir,
                                op == Language::Operator::Eq, ctx);
  } else if (lhs_type->is<type::Struct>() || rhs_type->is<type::Struct>()) {
    AST::FnArgs<std::pair<AST::Expression *, IR::Val>> args;
    args.pos_.reserve(2);
    args.pos_.emplace_back(
        chain_op->exprs[index].get(),
        chain_op->exprs[index]->type->is_big() ? PtrCallFix(lhs_ir) : lhs_ir);
    args.pos_.emplace_back(chain_op->exprs[index + 1].get(),
                           chain_op->exprs[index + 1]->type->is_big()
                               ? PtrCallFix(rhs_ir)
                               : rhs_ir);

    return EmitCallDispatch(args, chain_op->dispatch_tables_[index], type::Bool,
                            ctx);

  } else {
    switch (op) {
      case Language::Operator::Lt: return IR::Lt(lhs_ir, rhs_ir);
      case Language::Operator::Le: return IR::Le(lhs_ir, rhs_ir);
      case Language::Operator::Eq: return IR::Eq(lhs_ir, rhs_ir);
      case Language::Operator::Ne: return IR::Ne(lhs_ir, rhs_ir);
      case Language::Operator::Ge: return IR::Ge(lhs_ir, rhs_ir);
      case Language::Operator::Gt:
        return IR::Gt(lhs_ir, rhs_ir);
        // TODO case Language::Operator::And: cmp = lhs_ir; break;

      default: UNREACHABLE();
    }
  }
}

IR::Val AST::ChainOp::EmitIR(Context *ctx) {
  if (ops[0] == Language::Operator::Xor) {
    auto iter = exprs.begin();
    auto val = (*iter)->EmitIR(ctx);
    while (++iter != exprs.end()) {
      val = IR::Xor(std::move(val), (*iter)->EmitIR(ctx));
    }
    return val;
  } else if (ops[0] == Language::Operator::Or && type->is<type::Enum>()) {
    auto iter = exprs.begin();
    auto val = (*iter)->EmitIR(ctx);
    while (++iter != exprs.end()) {
      val = IR::Or(std::move(val), (*iter)->EmitIR(ctx));
    }
    return val;
  } else if (ops[0] == Language::Operator::And && type->is<type::Enum>()) {
    auto iter = exprs.begin();
    auto val = (*iter)->EmitIR(ctx);
    while (++iter != exprs.end()) {
      val = IR::And(std::move(val), (*iter)->EmitIR(ctx));
    }
    return val;
  } else if (ops[0] == Language::Operator::Or && type == type::Type_) {
    // TODO probably want to check that each expression is a type? What if I
    // overload | to take my own stuff and have it return a type?
    std::vector<IR::Val> args;
    args.reserve(exprs.size());
    for (const auto &expr : exprs) { args.push_back(expr->EmitIR(ctx)); }
    return IR::Variant(std::move(args));
  } else if (ops[0] == Language::Operator::And ||
             ops[0] == Language::Operator::Or) {
    auto land_block = IR::Func::Current->AddBlock();
    std::vector<IR::Val> phi_args;
    phi_args.reserve(2 * exprs.size());
    bool is_or = (ops[0] == Language::Operator::Or);
    for (size_t i = 0; i < exprs.size() - 1; ++i) {
      auto val = exprs[i]->EmitIR(ctx);

      auto next_block = IR::Func::Current->AddBlock();
      IR::CondJump(val, is_or ? land_block : next_block,
                   is_or ? next_block : land_block);
      phi_args.push_back(IR::Val::Block(IR::Block::Current));
      phi_args.push_back(IR::Val::Bool(is_or));

      IR::Block::Current = next_block;
    }

    phi_args.push_back(IR::Val::Block(IR::Block::Current));
    phi_args.push_back(exprs.back()->EmitIR(ctx));
    IR::UncondJump(land_block);

    IR::Block::Current = land_block;
    auto phi = IR::Phi(type::Bool);
    IR::Func::Current->SetArgs(phi, std::move(phi_args));
    return IR::Func::Current->Command(phi).reg();

  } else {
    if (ops.size() == 1) {
      auto lhs_ir = exprs[0]->EmitIR(ctx);
      auto rhs_ir = exprs[1]->EmitIR(ctx);
      auto val = EmitChainOpPair(this, 0, lhs_ir, rhs_ir, ctx);
      return val;

    } else {
      std::vector<IR::Val> phi_args;
      auto lhs_ir = exprs.front()->EmitIR(ctx);
      auto land_block = IR::Func::Current->AddBlock();
      for (size_t i = 0; i < ops.size() - 1; ++i) {
        auto rhs_ir = exprs[i + 1]->EmitIR(ctx);
        IR::Val cmp = EmitChainOpPair(this, i, lhs_ir, rhs_ir, ctx);

        phi_args.push_back(IR::Val::Block(IR::Block::Current));
        phi_args.push_back(IR::Val::Bool(false));
        auto next_block = IR::Func::Current->AddBlock();
        IR::CondJump(cmp, next_block, land_block);
        IR::Block::Current = next_block;
        lhs_ir = rhs_ir;
      }

      // Once more for the last element, but don't do a conditional jump.
      auto rhs_ir = exprs.back()->EmitIR(ctx);
      auto last_cmp =
          EmitChainOpPair(this, exprs.size() - 2, lhs_ir, rhs_ir, ctx);
      phi_args.push_back(IR::Val::Block(IR::Block::Current));
      phi_args.push_back(last_cmp);
      IR::UncondJump(land_block);

      IR::Block::Current = land_block;
      auto phi = IR::Phi(type::Bool);
      IR::Func::Current->SetArgs(phi, std::move(phi_args));
      return IR::Func::Current->Command(phi).reg();
    }
  }
}

IR::Val AST::CommaList::EmitIR(Context *) { UNREACHABLE(this); }
IR::Val AST::CommaList::EmitLVal(Context *) { NOT_YET(); }

IR::Val AST::GenericFunctionLiteral::EmitIR(Context *) {
  return IR::Val::GenFnLit(this);
}

IR::Val AST::FunctionLiteral::EmitIR(Context *ctx) {
  if (!ir_func_) {
    std::vector<std::pair<std::string, AST::Expression *>> args;
    args.reserve(inputs.size());
    for (const auto &input : inputs) {
      args.emplace_back(input->as<Declaration>().identifier->token,
                        input->as<Declaration>().init_val.get());
    }

    ir_func_ = ctx->mod_->AddFunc(this, std::move(args));
    ctx->mod_->to_complete_.push(this);
  }
  return IR::Val::FnLit(this);
}

void AST::FunctionLiteral::CompleteBody(Module *mod) {
  if (completed_) { return; }
  completed_ = true;

  Context ctx(mod);
  ctx.bound_constants_ = bound_constants_;
  statements->VerifyType(&ctx);
  statements->Validate(&ctx);
  limit_to(statements);
  stage_range_.low = EmitStage;

  if (stage_range_.high < EmitStage) { return; }
  if (type == type::Err) { return; }

  CURRENT_FUNC(ir_func_) {
    IR::Block::Current = ir_func_->entry();
    // Leave space for allocas that will come later (added to the entry
    // block).
    auto start_block   = IR::Func::Current->AddBlock();
    IR::Block::Current = start_block;

    // TODO arguments should be renumbered to not waste space on const values
    for (size_t i = 0; i < inputs.size(); ++i) {
      // TODO positional arguments
      if (inputs[i]->const_) {
        auto *val =
            AST::find(ctx.bound_constants_, inputs[i]->identifier->token);
        if (val) { inputs[i]->addr = *val; }
        continue;
      }
      inputs[i]->addr = IR::Func::Current->Argument(static_cast<i32>(i));
    }

    for (size_t i = 0; i < outputs.size(); ++i) {
      if (!outputs[i]->is<Declaration>()) { continue; }
      outputs[i]->as<Declaration>().addr =
          IR::Val::Ret(IR::ReturnValue(i), outputs[i]->type);
    }

    for (auto scope : fn_scope->innards_) {
      scope->ForEachDeclHere([](Declaration *decl) {
        if (decl->const_) {
          // Compute these values lazily in Identifier::EmitIR, because
          // otherwise we would have to figure out a valid ordering.
          return;
        }

        // TODO arg_val seems to go along with in_decl a lot. Is there some
        // reason for this that *should* be abstracted?
        if (decl->arg_val || decl->is<InDecl>()) { return; }
        ASSERT(decl->type != nullptr);
        decl->addr = IR::Alloca(decl->type);
      });
    }

    statements->VerifyType(&ctx);
    statements->Validate(&ctx);
    statements->EmitIR(&ctx);
    if (type->as<type::Function>().output.empty()) {
      // TODO even this is wrong. Figure out the right jumping strategy
      // between here and where you call SetReturn
      IR::ReturnJump();
    }

    IR::Block::Current = ir_func_->entry();
    IR::UncondJump(start_block);
  }
}

IR::Val AST::Statements::EmitIR(Context *ctx) {
  for (auto &stmt : content_) { stmt->EmitIR(ctx); }
  return IR::Val::None();
}

IR::Val AST::CodeBlock::EmitIR(Context *) {
  std::vector<IR::Val> args;
  auto copy = *this;
  if (auto *stmts = std::get_if<AST::Statements>(&copy.content_)) {
    stmts->SaveReferences(scope_, &args);
  }
  return IR::Contextualize(std::move(copy), std::move(args));
}

IR::Val AST::Identifier::EmitLVal(Context *ctx) {
  ASSERT(decl != nullptr);
  if (decl->addr == IR::Val::None()) { decl->EmitIR(ctx); }
  return decl->addr;
}

IR::Val AST::Unop::EmitLVal(Context *ctx) {
  ASSERT(static_cast<int>(op) == static_cast<int>(Language::Operator::At));
  return operand->EmitIR(ctx);
}

IR::Val AST::Import::EmitLVal(Context *ctx) { UNREACHABLE(); }

IR::Val AST::Binop::EmitLVal(Context *ctx) {
  switch (op) {
    case Language::Operator::Index:
      if (lhs->type->is<type::Array>()) {
        return IR::Index(lhs->EmitLVal(ctx), rhs->EmitIR(ctx));
      }
      [[fallthrough]];
    default: UNREACHABLE("Operator is ", static_cast<int>(op));
  }
}

IR::Val AST::StructLiteral::EmitIR(Context *ctx) {
  auto new_struct = IR::CreateStruct();
  for (const auto &field : fields_) {
    // TODO in initial value doesn't match type of field?
    // That should probably be handled elsewhere consistently with function
    // default args.
    IR::Val init_val = IR::Val::None();
    if (field->init_val) {
      field->init_val->assign_scope(scope_);
      field->init_val->Validate(ctx);
      init_val = field->init_val->EmitIR(ctx);
    }

    IR::Val field_type;
    if (field->type_expr) {
      field_type = field->type_expr->EmitIR(ctx);
    } else {
      ASSERT(nullptr != field->init_val.get());
      field_type = IR::Val::Type(field->init_val->type);
    }
    IR::InsertField(new_struct, field->identifier->token, std::move(field_type),
                    std::move(init_val));
  }
  return IR::FinalizeStruct(std::move(new_struct));
}
