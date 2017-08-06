#include "ir.h"

#include "../ast/ast.h"
#include "../scope.h"
#include "../type/type.h"
#include "../error_log.h"

#include <vector>

extern std::vector<Error> errors;

#define VERIFY_OR_EXIT                                                         \
  do {                                                                         \
    verify_types();                                                            \
    if (!errors.empty()) { return IR::Val::None(); }                           \
  } while (false)

extern IR::Val PtrCallFix(IR::Val v);
extern IR::Val Evaluate(AST::Expression *expr);
extern std::vector<IR::Val> global_vals;

// If the expression is a CommaList, apply the function to each expr. Otherwise
// call it on the expression itself.
static void ForEachExpr(AST::Expression *expr,
                 const std::function<void(size_t, AST::Expression *)> &fn) {
  if (expr->is<AST::CommaList>()) {
    const auto &exprs = ptr_cast<AST::CommaList>(expr)->exprs;
    for (size_t i = 0; i < exprs.size(); ++i) { fn(i, exprs[i].get()); }
  } else {
    fn(0, expr);
  }
}

IR::Val ErrorFunc() {
  static IR::Func *ascii_func_ = []() {
    auto fn = new IR::Func(Func(String, Code));
    CURRENT_FUNC(fn) {
      IR::Block::Current = fn->entry();
      // TODO
      IR::SetReturn(0, IR::Val::CodeBlock(nullptr));
      IR::Jump::Unconditional(fn->exit());

      IR::Block::Current = fn->exit();
      IR::Jump::Return();
    }
    fn->name = "error";
    return fn;
  }();
  return IR::Val::Func(ascii_func_);
}

IR::Val AsciiFunc() {
  static IR::Func *ascii_func_ = []() {
    auto fn = new IR::Func(Func(Uint, Char));
    CURRENT_FUNC(fn) {
      IR::Block::Current = fn->entry();
      IR::SetReturn(0, IR::Trunc(IR::Val::Arg(::Uint, 0)));
      IR::Jump::Unconditional(fn->exit());

      IR::Block::Current = fn->exit();
      IR::Jump::Return();
    }
    fn->name = "ascii";
    return fn;
  }();
  return IR::Val::Func(ascii_func_);
}

IR::Val OrdFunc() {
  static IR::Func *ord_func_ = []() {
    auto fn = new IR::Func(Func(Char, Uint));
    CURRENT_FUNC(fn) {
      IR::Block::Current = fn->entry();
      IR::SetReturn(0, IR::Extend(IR::Val::Arg(::Uint, 0)));
      IR::Jump::Unconditional(fn->exit());

      IR::Block::Current = fn->exit();
      IR::Jump::Return();
    }
    fn->name = "ord";
    return fn;
  }();
  return IR::Val::Func(ord_func_);
}

IR::Val AST::Access::EmitLVal() {
  VERIFY_OR_EXIT;

  auto val = operand->EmitLVal();
  while (val.type->is<Pointer>() &&
         !ptr_cast<Pointer>(val.type)->pointee->is_big()) {
    val = IR::Load(val);
  }

  ASSERT(val.type->is<Pointer>(), "");
  ASSERT(ptr_cast<Pointer>(val.type)->pointee->is<Struct>(), "");

  auto struct_type = ptr_cast<Struct>(ptr_cast<Pointer>(val.type)->pointee);
  return IR::Field(val, struct_type->field_name_to_num AT(member_name));
}

IR::Val AST::Access::EmitIR() {
  VERIFY_OR_EXIT;

  if (type->is<Enum>()) {
    return ptr_cast<Enum>(type)->EmitLiteral(member_name);
  } else {
    return PtrCallFix(EmitLVal());
  }
  return IR::Val::None();
}

IR::Val AST::Terminal::EmitIR() {
  VERIFY_OR_EXIT;
  return value;
}

IR::Val AST::Identifier::EmitIR() {
  VERIFY_OR_EXIT;
  ASSERT(decl, "No decl for identifier \"" + token + "\"");
  if (decl->arg_val) {
    return decl->addr;
  } else if (decl->is<InDecl>()) {
    auto* in_decl = ptr_cast<InDecl>(decl);
    if (in_decl->container->type->is<Array>()) {
      return PtrCallFix(EmitLVal());
    } else {
      return decl->addr;
    }

  } else if (type == Type_) {
    // TODO this is a hack and not entirely correct because perhaps types are
    // not const.
    return Evaluate(decl->init_val.get());
  } else {
    return PtrCallFix(EmitLVal());
  }
}

IR::Val AST::ArrayLiteral::EmitIR() {
  VERIFY_OR_EXIT;
  // TODO this allocation needs to be handled appropriately (in the entry block
  // perhaps).
  auto array_val = IR::Alloca(type);
  auto* data_type = ptr_cast<Array>(type)->data_type;
  for (size_t i = 0; i < elems.size(); ++i) {
    auto elem_i = IR::Index(array_val, IR::Val::Uint(i));
    Type::CallAssignment(scope_, data_type, data_type, elems[i]->EmitIR(),
                         elem_i);
  }
  return array_val;
}

IR::Val AST::For::EmitIR() {
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
              ptr_cast<Binop>(decl->container.get())->lhs->EmitIR());
        } else if (decl->container->is<Unop>()) {
          init_vals.push_back(
              ptr_cast<Unop>(decl->container.get())->operand->EmitIR());
        } else {
          NOT_YET();
        }
      } else if (decl->container->type->is<Array>()) {
        auto* array_type = ptr_cast<Array>(decl->container->type);
        if (array_type->fixed_length) {
          init_vals.push_back(
              IR::Index(decl->container->EmitLVal(), IR::Val::Uint(0)));
        } else {
          NOT_YET();
        }

      } else if (decl->container->type == Type_) {
        // TODO this conditional check on the line above is wrong
        IR::Val container_val = Evaluate(decl->container.get());
        if (container_val.as_type->is<Enum>()) {
          auto* enum_type = ptr_cast<Enum>(container_val.as_type);
          init_vals.push_back(enum_type->EmitInitialValue());
        } else {
          NOT_YET();
        }
      } else {
        NOT_YET();
      }
    }
    IR::Jump::Unconditional(phi);
  }

  std::vector<IR::Val> phis;
  phis.reserve(iterators.size());
  { // Phi block
    IR::Block::Current = phi;
    for (auto &decl : iterators) {
      if (decl->container->type == Type_) {
        NOT_YET();
      } else if (decl->container->type->is<RangeType>()) {
        phis.push_back(
            IR::Phi(ptr_cast<RangeType>(decl->container->type)->end_type));
      } else if (decl->container->type->is<Array>()) {
        phis.push_back(
            IR::Phi(Ptr(ptr_cast<Array>(decl->container->type)->data_type)));
      }
    }
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
      } else if (iter.type->is<Pointer>()) {
        incr_vals.push_back(IR::PtrIncr(iter, IR::Val::Uint(1)));
      } else {
        NOT_YET();
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
              ptr_cast<Binop>(decl->container.get())->rhs->EmitIR();
          cmp = IR::Le(reg, rhs_val);
        } else if (decl->container->is<Unop>()) {
          // TODO we should optimize this here rather then generate suboptimal
          // code and trust optimizations later on.
          cmp = IR::Val::Bool(true);
        } else {
          NOT_YET();
       }
      } else if (decl->container->type->is<Array>()) {
        auto* array_type = ptr_cast<Array>(decl->container->type);
        if (array_type->fixed_length) {
          cmp = IR::Ne(reg, IR::Index(decl->container->EmitLVal(),
                                      IR::Val::Uint(array_type->len)));
        } else {
          NOT_YET();
        }
      } else if (decl->container->type == Type_) {
        IR::Val container_val = Evaluate(decl->container.get());
        if (container_val.as_type->is<Enum>()) {
          // TODO I should not have to recalculate this here.
          auto* enum_type = ptr_cast<Enum>(container_val.as_type);

          cmp = IR::Le(reg,
                       IR::Val::Enum(enum_type, enum_type->members.size() - 1));
        } else {
          NOT_YET();
        }
      } else {
        NOT_YET();
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

    statements->EmitIR();

    // TODO destruct all decls
    IR::Jump::Unconditional(incr);
  }

  IR::Block::Current = exit;
  return IR::Val::None();
}

IR::Val AST::Case::EmitIR() {
  VERIFY_OR_EXIT;
  auto land = IR::Func::Current->AddBlock();

  ASSERT(!key_vals.empty(), "");
  std::vector<IR::Val> phi_args;
  phi_args.reserve(2 * key_vals.size());
  for (size_t i = 0; i < key_vals.size() - 1; ++i) {
    auto compute = IR::Func::Current->AddBlock();
    auto next    = IR::Func::Current->AddBlock();

    auto val = key_vals[i].first->EmitIR();
    IR::Jump::Conditional(val, compute, next);

    IR::Block::Current = compute;
    phi_args.push_back(IR::Val::Block(IR::Block::Current));
    auto result = key_vals[i].second->EmitIR();
    phi_args.push_back(result);
    IR::Jump::Unconditional(land);

    IR::Block::Current = next;
  }

  // Last entry
  phi_args.push_back(IR::Val::Block(IR::Block::Current));
  auto result = key_vals.back().second->EmitIR();
  phi_args.push_back(result);
  IR::Jump::Unconditional(land);

  IR::Block::Current = land;
  auto phi           = IR::Phi(type);
  IR::Func::Current->SetArgs(phi.as_reg, std::move(phi_args));
  return phi;
}

IR::Val AST::ScopeLiteral::EmitIR() {
  VERIFY_OR_EXIT;
  return IR::Val::Scope(this);
}

IR::Val AST::ScopeNode::EmitIR() {
  VERIFY_OR_EXIT;
  IR::Val scope_expr_val = Evaluate(scope_expr.get());
  ASSERT(scope_expr_val.type->is<Scope_Type>(), "");

  auto enter_fn = scope_expr_val.as_scope->enter_fn->init_val->EmitIR();
  ASSERT_NE(enter_fn, IR::Val::None());
  auto exit_fn = scope_expr_val.as_scope->exit_fn->init_val->EmitIR();
  ASSERT_NE(exit_fn, IR::Val::None());

  auto call_enter_result =
      IR::Call(enter_fn, expr ? std::vector<IR::Val>{expr->EmitIR()}
                              : std::vector<IR::Val>{});
  auto land_block  = IR::Func::Current->AddBlock();
  auto enter_block = IR::Func::Current->AddBlock();

  IR::Jump::Conditional(call_enter_result, enter_block, land_block);

  IR::Block::Current = enter_block;
  stmts->EmitIR();

  auto call_exit_result = IR::Call(exit_fn, {});
  IR::Jump::Conditional(call_exit_result, enter_block, land_block);

  IR::Block::Current = land_block;
  return IR::Val::None();
}

IR::Val AST::Declaration::EmitIR() {
  VERIFY_OR_EXIT;

  if (scope_ == Scope::Global) {
    if (addr != IR::Val::None()) { return IR::Val::None(); }

    // TODO these checks actually overlap and could be simplified.
    if (IsUninitialized()) {
      global_vals.emplace_back();
      global_vals.back().type = type;
      addr = IR::Val::GlobalAddr(global_vals.size() - 1, type);
    } else if (IsCustomInitialized()) {
      global_vals.push_back(Evaluate(init_val.get()));
      addr = IR::Val::GlobalAddr(global_vals.size() - 1, type);

    } else if (IsDefaultInitialized()) {
      // TODO if EmitInitialValue requires generating code, that would be bad.
      global_vals.push_back(type->EmitInitialValue());
      addr = IR::Val::GlobalAddr(global_vals.size() - 1, type);

    } else if (IsInferred()) {
      NOT_YET();

    } else {
      UNREACHABLE();
    }
    return IR::Val::None();
  } else {
    // For local variables the declaration determines where the initial value is
    // set, but the allocation has to be done much earlier. We do the allocation
    // in FunctionLiteral::EmitIR. Declaration::EmitIR is just used to set the
    // value.
    ASSERT_NE(addr, IR::Val::None());
    ASSERT(scope_->ContainingFnScope(), "");

    // TODO these checks actually overlap and could be simplified.
    if (IsUninitialized()) { return IR::Val::None(); }
    auto ir_init_val = IsCustomInitialized() ? init_val->EmitIR()
                                             : type->EmitInitialValue();
    // TODO these types do not always have to match. For example:
    // arr: [--; int] = [1, 4, 9]
    // Would indicate we want a dynamic array starting at size 3.
    Type::CallAssignment(scope_, type, type, ir_init_val, addr);
    return IR::Val::None();
  }
}

IR::Val AST::Unop::EmitIR() {
  VERIFY_OR_EXIT;

  switch (op) {
  case Language::Operator::Not:
  case Language::Operator::Sub: {
    return IR::Neg(operand->EmitIR());
  } break;
  case Language::Operator::Return: {
    if (operand->is<AST::CommaList>()) {
      bool all_types = true;
      std::vector<IR::Val> vals;
      for (const auto &expr : ptr_cast<AST::CommaList>(operand.get())->exprs) {
        vals.push_back(expr->EmitIR());
        if (expr->type != Type_) { all_types = false; }
      }

      if (all_types) {
        std::vector<Type*> types;
        for (const auto &val : vals) { types.push_back(val.as_type); }
        IR::SetReturn(0, IR::Val::Type(Tup(types)));
      } else {
        size_t i = 0;
        for (auto &val : vals) { IR::SetReturn(i++, std::move(val)); }
      }
    } else {
      auto val = operand->EmitIR();
      ASSERT(errors.empty(), "Errors in " + operand->to_string(0));
      IR::SetReturn(0, val);
    }

    ASSERT(scope_->is<ExecScope>(), "");
    // ptr_cast<BlockScope>(scope_)->MakeReturn(operand->EmitIR());
    IR::Jump::Unconditional(IR::BlockIndex{1});

    // TODO this is the right number but not implemented correctly.
    IR::Block::Current = IR::BlockIndex{1};
    IR::Jump::Return();
    return IR::Val::None();
  }
  case Language::Operator::Print: {
    ForEachExpr(operand.get(), +[](size_t, AST::Expression *expr) {
      if (expr->type->is<Primitive>() || expr->type->is<Pointer>()) {
        IR::Print(expr->EmitIR());
      } else {
        expr->type->EmitRepr(expr->EmitIR());
      }
    });

    return IR::Val::None();
  } break;
  case Language::Operator::And: return operand->EmitLVal();
  case Language::Operator::Eval:
    // TODO what if there's an error during evaluation?
    return Evaluate(operand.get());
  case Language::Operator::Generate: {
    auto val= Evaluate(operand.get());
    ASSERT_EQ(val.type, Code);
    val.as_code->stmts->assign_scope(scope_);
    val.as_code->stmts->EmitIR();
    return IR::Val::None();
  } break;
  case Language::Operator::Mul: return IR::Ptr(operand->EmitIR());
  case Language::Operator::At: return PtrCallFix(operand->EmitIR());
  default: UNREACHABLE("Operator is ", static_cast<int>(op));
  }
}

IR::Val AST::Binop::EmitIR() {
  VERIFY_OR_EXIT;
  switch (op) {
#define CASE(op_name)                                                          \
  case Language::Operator::op_name: {                                          \
    auto lhs_ir = lhs->EmitIR();                                               \
    auto rhs_ir = rhs->EmitIR();                                               \
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
    ASSERT(!rhs->is<AST::CommaList>(), "");
    auto lhs_ir = lhs->EmitIR();
    auto rhs_ir = rhs->EmitIR();
    return IR::Cast(lhs_ir, rhs_ir);
  } break;
  case Language::Operator::Call: {
    auto lhs_ir = lhs->EmitIR();
    std::vector<IR::Val> args;
    if (rhs) {
      ForEachExpr(rhs.get(), [&args](size_t, AST::Expression *expr) {
        args.push_back(expr->EmitIR());
      });
    }
    return IR::Call(lhs_ir, std::move(args));
  } break;
  case Language::Operator::Assign: {
    std::vector<IR::Val> rhs_vals;
    ForEachExpr(rhs.get(), [&rhs_vals](size_t, AST::Expression *expr) {
      rhs_vals.push_back(expr->EmitIR());
    });

    std::vector<IR::Val> lhs_lvals;
    ForEachExpr(lhs.get(), [&lhs_lvals](size_t, AST::Expression *expr) {
      lhs_lvals.push_back(expr->EmitLVal());
    });

    ASSERT_EQ(lhs_lvals.size(), rhs_vals.size());
    for (size_t i = 0;i < lhs_lvals.size(); ++i) {
      IR::Store(rhs_vals[i], lhs_lvals[i]);
    }
    return IR::Val::None();
  } break;
  case Language::Operator::OrEq: {
    auto land_block = IR::Func::Current->AddBlock();
    auto more_block = IR::Func::Current->AddBlock();

    auto lhs_val       = lhs->EmitIR();
    auto lhs_end_block = IR::Block::Current;
    IR::Jump::Conditional(lhs_val, land_block, more_block);

    IR::Block::Current = more_block;
    auto rhs_val       = rhs->EmitIR();
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

    auto lhs_val       = lhs->EmitIR();
    auto lhs_end_block = IR::Block::Current;
    IR::Jump::Conditional(lhs_val, more_block, land_block);

    IR::Block::Current = more_block;
    auto rhs_val       = rhs->EmitIR();
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
    auto lhs_lval = lhs->EmitLVal();                                           \
    auto rhs_ir   = rhs->EmitIR();                                             \
    return IR::Store(IR::op_name(PtrCallFix(lhs_lval), rhs_ir), lhs_lval);     \
  } break
    CASE_ASSIGN_EQ(Xor);
    CASE_ASSIGN_EQ(Add);
    CASE_ASSIGN_EQ(Sub);
    CASE_ASSIGN_EQ(Mul);
    CASE_ASSIGN_EQ(Div);
    CASE_ASSIGN_EQ(Mod);
#undef CASE_ASSIGN_EQ
  case Language::Operator::Index: return PtrCallFix(EmitLVal());
  default: UNREACHABLE(*this);
  }
}

IR::Val AST::ArrayType::EmitIR() {
  VERIFY_OR_EXIT;
  return IR::Array(length->EmitIR(), data_type->EmitIR());
}

IR::Val AST::ChainOp::EmitIR() {
  VERIFY_OR_EXIT;
  if (ops[0] == Language::Operator::Xor) {
    auto val = IR::Val::Bool(false);
    for (const auto &expr : exprs) {
      val = IR::Xor(val, expr->EmitIR());
    }
    return val;
  } else {
    std::vector<IR::BlockIndex> blocks;
    blocks.reserve(exprs.size());

    for (size_t i = 0; i < exprs.size(); ++i) {
      blocks.push_back(IR::Func::Current->AddBlock());
    }
    auto land_block = blocks.back();

    auto lhs_ir      = exprs[0]->EmitIR();
    auto start_block = IR::Block::Current;
    for (size_t i = 0; i < ops.size(); ++i) {
      auto rhs_ir = exprs[i + 1]->EmitIR();
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
      default: UNREACHABLE(*this);
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

  NOT_YET();
}

IR::Val AST::CommaList::EmitIR() { UNREACHABLE(); }
IR::Val AST::CommaList::EmitLVal() { NOT_YET(); }

IR::Val AST::FunctionLiteral::EmitIR() {
  VERIFY_OR_EXIT;
  // Verifying 'this' only verifies the declared functions type not the
  // internals. We need to do that here.
  statements->verify_types();
  if (!errors.empty()) { return IR::Val::None(); }

  CURRENT_FUNC(ir_func = new IR::Func(ptr_cast<Function>(type))) {
    IR::Block::Current = ir_func->entry();

    for (size_t i = 0; i < inputs.size(); ++i) {
      auto& arg = inputs[i];
      ASSERT_EQ(arg->addr, IR::Val::None());
      // This whole loop can be done on construction!
      arg->addr =
          IR::Val::Arg(arg->type->is_big() ? Ptr(arg->type) : arg->type, i);
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

    statements->EmitIR();
    IR::Jump::Unconditional(ir_func->exit());

    IR::Block::Current = ir_func->exit();
    IR::Jump::Return();
  }

  return IR::Val::Func(ir_func);
}

IR::Val AST::Statements::EmitIR() {
  VERIFY_OR_EXIT;
  for (auto &stmt : statements) { stmt->EmitIR(); }
  return IR::Val::None();
}

IR::Val AST::CodeBlock::EmitIR() {
  VERIFY_OR_EXIT;
  std::vector<IR::Val> args;
  stmts->contextualize(scope_, &args);
  return IR::Contextualize(this, std::move(args));
}

IR::Val AST::Identifier::EmitLVal() {
  VERIFY_OR_EXIT;
  ASSERT(decl != nullptr, "");

  if (decl->addr == IR::Val::None()) { decl->EmitIR(); }
  return decl->addr;
}

IR::Val AST::Unop::EmitLVal() {
  switch (op) {
  case Language::Operator::At:
    return operand->EmitIR();
  default: UNREACHABLE("Operator is ", static_cast<int>(op));
  }
}

IR::Val AST::Binop::EmitLVal() {
  switch (op) {
  case Language::Operator::Index:
    return IR::Index(lhs->EmitLVal(), rhs->EmitIR());
  default: UNREACHABLE("Operator is ", static_cast<int>(op));
  }
}
