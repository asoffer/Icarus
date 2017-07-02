#include "ir.h"

#include "../ast/ast.h"
#include "../scope.h"
#include "../type/type.h"

#define VERIFY_OR_EXIT                                                         \
  do {                                                                         \
    verify_types(errors);                                                      \
    if (!errors->empty()) { return IR::Val::None(); }                          \
  } while (false)

extern IR::Val PtrCallFix(IR::Val v);
extern IR::Val Evaluate(AST::Expression *expr);
extern std::vector<IR::Val> global_vals;

static IR::Val AsciiFunc() {
  static IR::Func *ascii_func_ = []() {
    auto fn = new IR::Func(Func(Uint, Char));
    CURRENT_FUNC(fn) {
      IR::Block::Current = fn->entry();
      IR::SetReturn(0, IR::Trunc(IR::Val::Arg(::Uint, 0)));
      IR::Jump::Unconditional(fn->exit());

      IR::Block::Current = fn->exit();
      IR::Jump::Return();
    }
    return fn;
  }();
  return IR::Val::Func(ascii_func_);
}

static IR::Val OrdFunc() {
  static IR::Func *ord_func_ = []() {
    auto fn = new IR::Func(Func(Char, Uint));
    CURRENT_FUNC(fn) {
      IR::Block::Current = fn->entry();
      IR::SetReturn(0, IR::Extend(IR::Val::Arg(::Uint, 0)));
      IR::Jump::Unconditional(fn->exit());

      IR::Block::Current = fn->exit();
      IR::Jump::Return();
    }
    return fn;
  }();
  return IR::Val::Func(ord_func_);
}

IR::Val AST::Access::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT;

  if (type->is<Enum>()) {
    return ptr_cast<Enum>(type)->EmitLiteral(member_name);
  } else if (type->is<Struct>()) {
    NOT_YET;
  } else {
    NOT_YET;
  }
  return IR::Val::None();
}

IR::Val AST::Terminal::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT;
  switch (terminal_type) {
  case Language::Terminal::Char:
  case Language::Terminal::Int:
  case Language::Terminal::Real:
  case Language::Terminal::Type:
  case Language::Terminal::Uint:
  case Language::Terminal::True:
  case Language::Terminal::False: return value;
  case Language::Terminal::ASCII: return AsciiFunc();
  case Language::Terminal::Ord: return OrdFunc();
  case Language::Terminal::Return: IR::Jump::Return(); return IR::Val::None();
  default: NOT_YET;
  }
}

IR::Val AST::Identifier::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT;
  ASSERT(decl, "No decl for identifier \"" + token + "\"");
  if (decl->arg_val || decl->is<InDecl>()) {
    return decl->addr;
  } else {
    return PtrCallFix(EmitLVal(errors));
  }
}

// TODO
IR::Val AST::ArrayLiteral::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT;
  return IR::Val::None();
}

IR::Val AST::For::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT;

  auto init       = IR::Func::Current->AddBlock();
  auto incr       = IR::Func::Current->AddBlock();
  auto phi        = IR::Func::Current->AddBlock();
  auto cond       = IR::Func::Current->AddBlock();
  auto body_entry = IR::Func::Current->AddBlock();
  auto exit       = IR::Func::Current->AddBlock();

  IR::Jump::Unconditional(init);

  std::vector<IR::Val> init_vals;
  init_vals.reserve(iterators.size());
  { // Init block
    IR::Block::Current = init;
    for (auto& decl : iterators) {
      if (decl->container->type->is<RangeType>()) {
        if (decl->container->is<Binop>()) {
          init_vals.push_back(
              ptr_cast<Binop>(decl->container.get())->lhs->EmitIR(errors));
        } else if (decl->container->is<Unop>()) {
          init_vals.push_back(
              ptr_cast<Unop>(decl->container.get())->operand->EmitIR(errors));
        } else {
          NOT_YET;
        }
      } else if (decl->container->type->is<Type>()) {
        IR::Val container_val = Evaluate(decl->container.get());
        if (container_val.as_type->is<Enum>()) {
          auto* enum_type = ptr_cast<Enum>(container_val.as_type);
          init_vals.push_back(enum_type->EmitInitialValue());
        } else {
          NOT_YET;
        }
      } else {
        NOT_YET;
      }
    }
    IR::Jump::Unconditional(phi);
  }

  std::vector<IR::Val> phis;
  phis.reserve(iterators.size());
  { // Phi block
    IR::Block::Current = phi;
    for (auto& decl : iterators) { phis.push_back(IR::Phi(decl->type)); }
    IR::Jump::Unconditional(cond);
  }

  std::vector<IR::Val> incr_vals;
  incr_vals.reserve(iterators.size());
  { // Incr block
    IR::Block::Current = incr;
    for (auto iter : phis) {
      if (iter.type == Int) {
        incr_vals.push_back(IR::Add(iter, IR::Val::Int(1)));
      } else if (iter.type == Uint) {
        incr_vals.push_back(IR::Add(iter, IR::Val::Uint(1)));
      } else if (iter.type == Char) {
        incr_vals.push_back(IR::Add(iter, IR::Val::Char(1)));
      } else if (iter.type->is<Enum>()) {
        incr_vals.push_back(
            IR::Add(iter, IR::Val::Enum(ptr_cast<Enum>(iter.type), 1)));
      } else {
        NOT_YET;
      }
    }
    IR::Jump::Unconditional(phi);
  }

  { // Complete phi definition
    for (size_t i = 0; i < iterators.size(); ++i) {
      IR::Func::Current->SetArgs(phis[i].as_reg,
                                 {IR::Val::Block(init), init_vals[i],
                                  IR::Val::Block(incr), incr_vals[i]});
      iterators[i]->addr = phis[i];
    }
  }

  { // Cond block
    IR::Block::Current = cond;
    for (size_t i = 0; i < iterators.size(); ++i) {
      auto* decl = iterators[i].get();
      auto reg  = phis[i];
      auto next = IR::Func::Current->AddBlock();
      IR::Val cmp;
      if (decl->container->type->is<RangeType>()) {
        if (decl->container->is<Binop>()) {
          auto rhs_val =
              ptr_cast<Binop>(decl->container.get())->rhs->EmitIR(errors);
          cmp = IR::Le(reg, rhs_val);
        } else if (decl->container->is<Unop>()) {
          // TODO we should optimize this here rather then generate suboptimal
          // code and trust optimizations later on.
          cmp = IR::Val::Bool(true);
        } else {
          NOT_YET;
        }
      } else if (decl->container->type->is<Type>()) {
        IR::Val container_val = Evaluate(decl->container.get());
        if (container_val.as_type->is<Enum>()) {
          // TODO I should not have to recalculate this here.
          auto* enum_type = ptr_cast<Enum>(container_val.as_type);
          cmp             = IR::Le(reg,
                       IR::Val::Enum(enum_type, enum_type->members.size() - 1));
        } else {
          NOT_YET;
        }
      } else {
        NOT_YET;
      }

      IR::Jump::Conditional(cmp, next, exit);
      IR::Block::Current = next;
    }
    IR::Jump::Unconditional(body_entry);
  }

  { // Body
    IR::Block::Current = body_entry;
    for (auto& decl : for_scope->decls_) {
      (void)decl;
      // TODO initialize all decls
    }

    statements->EmitIR(errors);

    // TODO destruct all decls
    IR::Jump::Unconditional(incr);
  }

  IR::Block::Current = exit;
  return IR::Val::None();
}

IR::Val AST::Case::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT;
  auto land = IR::Func::Current->AddBlock();

  ASSERT(!key_vals.empty(), "");
  std::vector<IR::Val> phi_args;
  phi_args.reserve(2 * key_vals.size());
  for (size_t i = 0; i < key_vals.size() - 1; ++i) {
    auto compute = IR::Func::Current->AddBlock();
    auto next    = IR::Func::Current->AddBlock();

    auto val = key_vals[i].first->EmitIR(errors);
    IR::Jump::Conditional(val, compute, next);

    IR::Block::Current = compute;
    phi_args.push_back(IR::Val::Block(IR::Block::Current));
    auto result = key_vals[i].second->EmitIR(errors);
    phi_args.push_back(result);
    IR::Jump::Unconditional(land);

    IR::Block::Current = next;
  }

  // Last entry
  phi_args.push_back(IR::Val::Block(IR::Block::Current));
  auto result = key_vals.back().second->EmitIR(errors);
  phi_args.push_back(result);
  IR::Jump::Unconditional(land);

  IR::Block::Current = land;
  auto phi           = IR::Phi(Bool);
  IR::Func::Current->SetArgs(phi.as_reg, std::move(phi_args));
  return phi;
}

IR::Val AST::ScopeLiteral::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT;
  return IR::Val::Scope(this);
}

IR::Val AST::ScopeNode::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT;
  IR::Val scope_expr_val = Evaluate(scope_expr.get());
  ASSERT(scope_expr_val.type->is<Scope_Type>(), "");

  auto enter_fn = scope_expr_val.as_scope->enter_fn->init_val->EmitIR(errors);
  ASSERT(enter_fn != IR::Val::None(), "");
  auto exit_fn = scope_expr_val.as_scope->exit_fn->init_val->EmitIR(errors);
  ASSERT(exit_fn != IR::Val::None(), "");

  auto call_enter_result = (expr == nullptr)
                               ? IR::Call(enter_fn, {})
                               : IR::Call(enter_fn, {expr->EmitIR(errors)});
  auto land_block  = IR::Func::Current->AddBlock();
  auto enter_block = IR::Func::Current->AddBlock();

  IR::Jump::Conditional(call_enter_result, enter_block, land_block);

  IR::Block::Current = enter_block;
  stmts->EmitIR(errors);

  auto call_exit_result = IR::Call(exit_fn, {});
  IR::Jump::Conditional(call_exit_result, enter_block, land_block);

  IR::Block::Current = land_block;
  return IR::Val::None();
}

IR::Val AST::Declaration::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT;
  if (scope_ == Scope::Global) {
    ASSERT(addr == IR::Val::None(), "");
    // TODO these checks actually overlap and could be simplified.

    addr = IR::Val::GlobalAddr(global_vals.size(), type);
    if (IsUninitialized()) {
      global_vals.emplace_back();
      global_vals.back().type = type;

    } else if (IsCustomInitialized()) {
      global_vals.push_back(Evaluate(init_val.get()));

    } else if (IsDefaultInitialized()) {
      // TODO if EmitInitialValue requires generating code, that would be bad.
      global_vals.push_back(type->EmitInitialValue());

    } else if (IsInferred()) {
      NOT_YET;

    } else {
      UNREACHABLE;
    }
    return IR::Val::None();
  } else {
    // For local variables the declaration determines where the initial value is
    // set, but the allocation has to be done much earlier. We do the allocation
    // in FunctionLiteral::EmitIR. Declaration::EmitIR is just used to set the
    // value.
    ASSERT(addr != IR::Val::None(), "");
    ASSERT(scope_->ContainingFnScope(), "");
    // TODO these checks actually overlap and could be simplified.
    if (IsUninitialized()) { return IR::Val::None(); }
    auto ir_init_val = IsCustomInitialized() ? init_val->EmitIR(errors)
                                             : type->EmitInitialValue();
    return IR::Store(ir_init_val, addr);
  }
}

IR::Val AST::Unop::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT;

  switch (op) {
  case Language::Operator::Not:
  case Language::Operator::Sub: {
    return IR::Neg(operand->EmitIR(errors));
  } break;
  case Language::Operator::Return: {
    if (operand->is_comma_list()) {
      const auto &exprs = ptr_cast<AST::ChainOp>(operand.get())->exprs;
      for (size_t i = 0; i < exprs.size(); ++i) {
        IR::SetReturn(i, exprs[i]->EmitIR(errors));
      }
    } else {
      IR::SetReturn(0, operand->EmitIR(errors));
    }

    ASSERT(scope_->is<ExecScope>(), "");
    // ptr_cast<BlockScope>(scope_)->MakeReturn(operand->EmitIR(errors));
    IR::Jump::Unconditional(IR::BlockIndex{1});

    // TODO this is the right number but not implemented correctly.
    IR::Block::Current = IR::BlockIndex{1};
    IR::Jump::Return();
    return IR::Val::None();
  }
  case Language::Operator::Print: {
    auto print = +[](AST::Expression *expr, std::vector<Error> *errors) {
      if (expr->type->is<Primitive>() || expr->type->is<Pointer>()) {
        IR::Print(expr->EmitIR(errors));
      } else {
        expr->type->EmitRepr(expr->EmitIR(errors));
      }
    };

    if (operand->is_comma_list()) {
      for (auto &expr : ptr_cast<AST::ChainOp>(operand.get())->exprs) {
        print(expr.get(), errors);
      }
    } else {
      print(operand.get(), errors);
    }
    return IR::Val::None();
  } break;
  case Language::Operator::And: return operand->EmitLVal(errors);
  case Language::Operator::Eval:
    // TODO what if there's an error during evaluation?
    return Evaluate(operand.get());
  case Language::Operator::Generate: NOT_YET;
  case Language::Operator::Mul: return IR::Ptr(operand->EmitIR(errors));
  case Language::Operator::At: return PtrCallFix(operand->EmitIR(errors));
  default: {
    std::cerr << "Operator is " << static_cast<int>(op) << std::endl;
    UNREACHABLE;
  }
  }
}

IR::Val AST::Binop::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT;
  switch (op) {
#define CASE(op_name)                                                          \
  case Language::Operator::op_name: {                                          \
    auto lhs_ir = lhs->EmitIR(errors);                                         \
    auto rhs_ir = rhs->EmitIR(errors);                                         \
    return IR::op_name(lhs_ir, rhs_ir);                                        \
  } break
    CASE(Add);
    CASE(Sub);
    CASE(Mul);
    CASE(Div);
    CASE(Mod);
    CASE(Arrow);
#undef CASE
  case Language::Operator::Cast: {
    ASSERT(!rhs->is_comma_list(), "");
    auto lhs_ir = lhs->EmitIR(errors);
    auto rhs_ir = rhs->EmitIR(errors);
    return IR::Cast(lhs_ir, rhs_ir);
  } break;
  case Language::Operator::Call: {
    auto lhs_ir = lhs->EmitIR(errors);
    std::vector<IR::Val> args;
    if (!rhs) {
      ;
    } else if (rhs->is_comma_list()) {
      auto rhs_comma_list = ptr_cast<ChainOp>(rhs.get());
      args.reserve(rhs_comma_list->exprs.size());
      for (auto& expr : rhs_comma_list->exprs) {
        args.push_back(expr->EmitIR(errors));
      }
    } else {
      args.push_back(rhs->EmitIR(errors));
    }
    return IR::Call(lhs_ir, std::move(args));
  } break;
  case Language::Operator::Assign: {
    auto lhs_lval = lhs->EmitLVal(errors);
    auto rhs_ir   = rhs->EmitIR(errors);
    return IR::Store(rhs_ir, lhs_lval);
  } break;
  case Language::Operator::OrEq: {
    auto land_block = IR::Func::Current->AddBlock();
    auto more_block = IR::Func::Current->AddBlock();

    auto lhs_val       = lhs->EmitIR(errors);
    auto lhs_end_block = IR::Block::Current;
    IR::Jump::Conditional(lhs_val, land_block, more_block);

    IR::Block::Current = more_block;
    auto rhs_val       = rhs->EmitIR(errors);
    auto rhs_end_block = IR::Block::Current;
    IR::Jump::Unconditional(land_block);

    IR::Block::Current = land_block;

    auto phi = IR::Phi(Bool);
    IR::Func::Current->SetArgs(
        phi.as_reg, {IR::Val::Block(lhs_end_block), IR::Val::Bool(true),
                     IR::Val::Block(rhs_end_block), rhs_val});
    return phi;
  } break;
  case Language::Operator::AndEq: {
    auto land_block = IR::Func::Current->AddBlock();
    auto more_block = IR::Func::Current->AddBlock();

    auto lhs_val       = lhs->EmitIR(errors);
    auto lhs_end_block = IR::Block::Current;
    IR::Jump::Conditional(lhs_val, more_block, land_block);

    IR::Block::Current = more_block;
    auto rhs_val       = rhs->EmitIR(errors);
    auto rhs_end_block = IR::Block::Current;
    IR::Jump::Unconditional(land_block);

    IR::Block::Current = land_block;

    auto phi = IR::Phi(Bool);
    IR::Func::Current->SetArgs(
        phi.as_reg, {IR::Val::Block(lhs_end_block), IR::Val::Bool(false),
                     IR::Val::Block(rhs_end_block), rhs_val});
    return phi;
  } break;
#define CASE_ASSIGN_EQ(op_name)                                                \
  case Language::Operator::op_name##Eq: {                                      \
    auto lhs_lval = lhs->EmitLVal(errors);                                     \
    auto rhs_ir   = rhs->EmitIR(errors);                                       \
    return IR::Store(IR::op_name(PtrCallFix(lhs_lval), rhs_ir), lhs_lval);     \
  } break
    CASE_ASSIGN_EQ(Xor);
    CASE_ASSIGN_EQ(Add);
    CASE_ASSIGN_EQ(Sub);
    CASE_ASSIGN_EQ(Mul);
    CASE_ASSIGN_EQ(Div);
    CASE_ASSIGN_EQ(Mod);
#undef CASE_ASSIGN_EQ
  default: {
    std::cerr << *this << std::endl;
    UNREACHABLE;
  }
  }
}

IR::Val AST::ArrayType::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT;
  return IR::Array(length->EmitIR(errors), data_type->EmitIR(errors));
}

IR::Val AST::ChainOp::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT;
  ASSERT(!is_comma_list(), "");
  if (ops[0] == Language::Operator::Xor) {
    auto val = IR::Val::Bool(false);
    for (const auto &expr : exprs) {
      val = IR::Xor(val, expr->EmitIR(errors));
    }
    return val;
  } else {
    std::vector<IR::BlockIndex> blocks;
    blocks.reserve(exprs.size());

    for (size_t i = 0; i < exprs.size(); ++i) {
      blocks.push_back(IR::Func::Current->AddBlock());
    }
    auto land_block = blocks.back();

    auto lhs_ir      = exprs[0]->EmitIR(errors);
    auto start_block = IR::Block::Current;
    for (size_t i = 0; i < ops.size(); ++i) {
      auto rhs_ir = exprs[i + 1]->EmitIR(errors);
      IR::Val cmp;
      switch (ops[i]) {
      case Language::Operator::Lt: cmp = IR::Lt(lhs_ir, rhs_ir); break;
      case Language::Operator::Le: cmp = IR::Le(lhs_ir, rhs_ir); break;
      case Language::Operator::Eq: cmp = IR::Eq(lhs_ir, rhs_ir); break;
      case Language::Operator::Ne: cmp = IR::Ne(lhs_ir, rhs_ir); break;
      case Language::Operator::Ge: cmp = IR::Ge(lhs_ir, rhs_ir); break;
      case Language::Operator::Gt: cmp = IR::Gt(lhs_ir, rhs_ir); break;
      case Language::Operator::And: {
        cmp = lhs_ir;
      } break;
      default: std::cerr << *this << std::endl; UNREACHABLE;
      }
      IR::Jump::Conditional(cmp, blocks[i], land_block);
      IR::Block::Current = blocks[i];
      lhs_ir             = rhs_ir;
    }

    IR::Jump::Unconditional(land_block);
    IR::Block::Current = land_block;
    std::vector<IR::Val> phi_args;
    phi_args.push_back(IR::Val::Block(start_block));
    phi_args.push_back(IR::Val::Bool(false));
    for (size_t i = 0; i < blocks.size() - 2; ++i) {
      phi_args.push_back(IR::Val::Block(blocks[i]));
      phi_args.push_back(IR::Val::Bool(false));
    }
    phi_args.push_back(IR::Val::Block(blocks[blocks.size() - 2]));
    phi_args.push_back(IR::Val::Bool(true));

    auto phi = IR::Phi(Bool);
    IR::Func::Current->SetArgs(phi.as_reg, std::move(phi_args));
    return phi;
  }

  NOT_YET;
}

IR::Val AST::FunctionLiteral::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT;
  // Verifying 'this' only verifies the declared functions type not the
  // internals. We need to do that here.
  statements->verify_types(errors);
  if (!errors->empty()) { return IR::Val::None(); }

  CURRENT_FUNC(ir_func = new IR::Func(type)) {
    IR::Block::Current = ir_func->entry();

    for (size_t i = 0; i < inputs.size(); ++i) {
      auto& arg = inputs[i];
      ASSERT(arg->addr == IR::Val::None(), "");
      // This whole loop can be done on construction!
      arg->addr = IR::Val::Arg(arg->type, i);
    }

    for (auto scope : fn_scope->innards_) {
      for (auto& decl : scope->decls_) {
        // TODO arg_val seems to go along with in_decl a lot. Is there some
        // reason for this that *should* be abstracted?
        if (decl->arg_val || decl->is<InDecl>()) { continue; }
        ASSERT(decl->type, "");
        decl->addr = IR::Alloca(decl->type);
      }
    }

    statements->EmitIR(errors);
    IR::Jump::Unconditional(ir_func->exit());

    IR::Block::Current = ir_func->exit();
    IR::Jump::Return();
  }

  return IR::Val::Func(ir_func);
}

IR::Val AST::Statements::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT;
  for (auto &stmt : statements) { stmt->EmitIR(errors); }
  return IR::Val::None();
}

IR::Val AST::CodeBlock::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT;
  return IR::Val::CodeBlock(this);
}

IR::Val AST::Identifier::EmitLVal(std::vector<Error> *errors) {
  VERIFY_OR_EXIT;
  ASSERT(decl, "");
  ASSERT(decl->addr != IR::Val::None(), decl->to_string(0));
  return decl->addr;
}
