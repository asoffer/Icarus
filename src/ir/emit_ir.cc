#include "ir.h"

#include "../ast/ast.h"
#include "../error_log.h"
#include "../scope.h"
#include "../type/type.h"

#include <vector>

#define VERIFY_OR_EXIT                                                         \
  do {                                                                         \
    verify_types();                                                            \
    if (ErrorLog::NumErrors() != 0) { return IR::Val::None(); }                \
  } while (false)

extern IR::Val PtrCallFix(IR::Val v);
extern IR::Val Evaluate(AST::Expression *expr);
extern std::vector<IR::Val> global_vals;

// If the expression is a CommaList, apply the function to each expr. Otherwise
// call it on the expression itself.
void ForEachExpr(AST::Expression *expr,
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
    auto fn = new IR::Func(Func(String, Code), {{"", nullptr}});
    CURRENT_FUNC(fn) {
      IR::Block::Current = fn->entry();
      // TODO
      IR::SetReturn(IR::ReturnValue{0}, IR::Val::CodeBlock(nullptr));
      IR::ReturnJump();
    }
    fn->name = "error";
    return fn;
  }();
  return IR::Val::Func(ascii_func_);
}

IR::Val AsciiFunc() {
  static IR::Func *ascii_func_ = []() {
    auto fn = new IR::Func(Func(Uint, Char), {{"", nullptr}});
    CURRENT_FUNC(fn) {
      IR::Block::Current = fn->entry();
      IR::SetReturn(IR::ReturnValue{0}, IR::Trunc(fn->Argument(0)));
      IR::ReturnJump();
    }
    fn->name = "ascii";
    return fn;
  }();
  return IR::Val::Func(ascii_func_);
}

IR::Val OrdFunc() {
  static IR::Func *ord_func_ = []() {
    auto fn = new IR::Func(Func(Char, Uint), {{"", nullptr}});
    CURRENT_FUNC(fn) {
      IR::Block::Current = fn->entry();
      IR::SetReturn(IR::ReturnValue{0}, IR::Extend(fn->Argument(0)));
      IR::ReturnJump();
    }
    fn->name = "ord";
    return fn;
  }();
  return IR::Val::Func(ord_func_);
}

IR::Val AST::Access::EmitLVal(IR::Cmd::Kind kind) {
  VERIFY_OR_EXIT;

  auto val = operand->EmitLVal(kind);
  while (val.type->is<Pointer>() &&
         !ptr_cast<Pointer>(val.type)->pointee->is_big()) {
    val = IR::Load(val);
  }

  ASSERT_TYPE(Pointer, val.type);
  ASSERT_TYPE(Struct, ptr_cast<Pointer>(val.type)->pointee);

  auto *struct_type = &val.type->as<Pointer>().pointee->as<Struct>();
  return IR::Field(val, struct_type->field_name_to_num AT(member_name));
}

IR::Val AST::CallArgs::EmitIR(IR::Cmd::Kind) {
  UNREACHABLE("Handled at Operator::Call site explicitly");
}

IR::Val AST::Access::EmitIR(IR::Cmd::Kind kind) {
  VERIFY_OR_EXIT;

  if (type->is<Enum>()) {
    return type->as<Enum>().EmitLiteral(member_name);
  } else {
    return PtrCallFix(EmitLVal(kind));
  }
  return IR::Val::None();
}

IR::Val AST::Terminal::EmitIR(IR::Cmd::Kind) {
  VERIFY_OR_EXIT;
  return value;
}

IR::Val AST::Identifier::EmitIR(IR::Cmd::Kind kind) {
  VERIFY_OR_EXIT;
  ASSERT(decl, "No decl for identifier \"" + token + "\"");

  if (kind == IR::Cmd::Kind::PostCondition &&
      decl->addr.value.is<IR::ReturnValue>()) {
    Type *input =
        scope_->ContainingFnScope()->fn_lit->type->as<Function>().input;
    i32 num_args = input->is<Tuple>()
                       ? static_cast<i32>(input->as<Tuple>().entries.size())
                       : 1;
    return IR::Val::Reg(
        IR::Register{
            num_args +
            static_cast<i32>(decl->addr.value.as<IR::ReturnValue>().value)},
        type);
  }

  if (decl->const_ || decl->arg_val) {
    return decl->addr;
  } else if (decl->is<InDecl>()) {
    auto *in_decl = ptr_cast<InDecl>(decl);
    if (in_decl->container->type->is<Array>()) {
      return PtrCallFix(EmitLVal(kind));
    } else {
      return decl->addr;
    }

  } else if (type == Type_) {
    // TODO this is a hack and not entirely correct because perhaps types are
    // not const.
    return Evaluate(decl->init_val.get());
  } else {
    return PtrCallFix(EmitLVal(kind));
  }
}

IR::Val AST::ArrayLiteral::EmitIR(IR::Cmd::Kind kind) {
  VERIFY_OR_EXIT;
  auto array_val  = IR::Alloca(type);
  auto *data_type = ptr_cast<Array>(type)->data_type;
  for (size_t i = 0; i < elems.size(); ++i) {
    auto elem_i = IR::Index(array_val, IR::Val::Uint(i));
    Type::EmitMoveInit(data_type, data_type, elems[i]->EmitIR(kind), elem_i);
  }
  return array_val;
}

IR::Val AST::For::EmitIR(IR::Cmd::Kind kind) {
  VERIFY_OR_EXIT;

  auto init       = IR::Func::Current->AddBlock();
  auto incr       = IR::Func::Current->AddBlock();
  auto phi        = IR::Func::Current->AddBlock();
  auto cond       = IR::Func::Current->AddBlock();
  auto body_entry = IR::Func::Current->AddBlock();
  auto exit       = IR::Func::Current->AddBlock();

  IR::UncondJump(init);

  std::vector<IR::Val> init_vals;
  init_vals.reserve(iterators.size());
  { // Init block
    IR::Block::Current = init;
    for (auto &decl : iterators) {
      if (decl->container->type->is<RangeType>()) {
        if (decl->container->is<Binop>()) {
          init_vals.push_back(
              ptr_cast<Binop>(decl->container.get())->lhs->EmitIR(kind));
        } else if (decl->container->is<Unop>()) {
          init_vals.push_back(
              ptr_cast<Unop>(decl->container.get())->operand->EmitIR(kind));
        } else {
          NOT_YET();
        }
      } else if (decl->container->type->is<Array>()) {
        init_vals.push_back(
            IR::Index(decl->container->EmitLVal(kind), IR::Val::Uint(0)));

      } else if (decl->container->type == Type_) {
        // TODO this conditional check on the line above is wrong
        IR::Val container_val = Evaluate(decl->container.get());
        if (container_val.value.as<::Type *>()->is<Enum>()) {
          auto *enum_type = ptr_cast<Enum>(container_val.value.as<::Type *>());
          init_vals.push_back(enum_type->EmitInitialValue());
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
  { // Phi block
    IR::Block::Current = phi;
    for (auto &decl : iterators) {
      if (decl->container->type == Type_) {
        IR::Val container_val = Evaluate(decl->container.get());
        if (container_val.value.as<::Type *>()->is<Enum>()) {
          phis.push_back(
              IR::Phi(ptr_cast<Enum>(container_val.value.as<::Type *>())));
        } else {
          NOT_YET();
        }
      } else if (decl->container->type->is<RangeType>()) {
        phis.push_back(
            IR::Phi(ptr_cast<RangeType>(decl->container->type)->end_type));
      } else if (decl->container->type->is<Array>()) {
        phis.push_back(
            IR::Phi(Ptr(ptr_cast<Array>(decl->container->type)->data_type)));
      } else {
        NOT_YET(*decl->container->type);
      }
    }
    IR::UncondJump(cond);
  }

  std::vector<IR::Val> incr_vals;
  incr_vals.reserve(iterators.size());
  { // Incr block
    IR::Block::Current = incr;
    for (auto iter : phis) {
      auto phi_reg = IR::Func::Current->Command(iter).reg();
      if (phi_reg.type == Int) {
        incr_vals.push_back(IR::Add(phi_reg, IR::Val::Int(1)));
      } else if (phi_reg.type == Uint) {
        incr_vals.push_back(IR::Add(phi_reg, IR::Val::Uint(1)));
      } else if (phi_reg.type == Char) {
        incr_vals.push_back(IR::Add(phi_reg, IR::Val::Char(1)));
      } else if (phi_reg.type->is<Enum>()) {
        incr_vals.push_back(
            IR::Add(phi_reg, IR::Val::Enum(ptr_cast<Enum>(phi_reg.type), 1)));
      } else if (phi_reg.type->is<Pointer>()) {
        incr_vals.push_back(IR::PtrIncr(phi_reg, IR::Val::Uint(1)));
      } else {
        NOT_YET();
      }
    }
    IR::UncondJump(phi);
  }

  { // Complete phi definition
    for (size_t i = 0; i < iterators.size(); ++i) {
      IR::Func::Current->SetArgs(phis[i], {IR::Val::Block(init), init_vals[i],
                                           IR::Val::Block(incr), incr_vals[i]});
      iterators[i]->addr = IR::Func::Current->Command(phis[i]).reg();
    }
  }

  { // Cond block
    IR::Block::Current = cond;
    for (size_t i = 0; i < iterators.size(); ++i) {
      auto *decl = iterators[i].get();
      auto reg   = IR::Func::Current->Command(phis[i]).reg();
      auto next  = IR::Func::Current->AddBlock();
      IR::Val cmp;
      if (decl->container->type->is<RangeType>()) {
        if (decl->container->is<Binop>()) {
          auto rhs_val =
              ptr_cast<Binop>(decl->container.get())->rhs->EmitIR(kind);
          cmp = IR::Le(reg, rhs_val);
        } else if (decl->container->is<Unop>()) {
          // TODO we should optimize this here rather then generate suboptimal
          // code and trust optimizations later on.
          cmp = IR::Val::Bool(true);
        } else {
          NOT_YET();
        }
      } else if (decl->container->type->is<Array>()) {
        auto *array_type = ptr_cast<Array>(decl->container->type);
        cmp = IR::Ne(reg, IR::Index(decl->container->EmitLVal(kind),
                                    IR::Val::Uint(array_type->len)));
      } else if (decl->container->type == Type_) {
        IR::Val container_val = Evaluate(decl->container.get());
        if (container_val.value.as<::Type *>()->is<Enum>()) {
          // TODO I should not have to recalculate this here.
          auto *enum_type = ptr_cast<Enum>(container_val.value.as<::Type *>());

          cmp = IR::Le(reg,
                       IR::Val::Enum(enum_type, enum_type->members.size() - 1));
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

  { // Body
    IR::Block::Current = body_entry;

    for_scope->Enter();
    statements->EmitIR(kind);
    for_scope->Exit();

    IR::UncondJump(incr);
  }

  IR::Block::Current = exit;
  return IR::Val::None();
}

IR::Val AST::Case::EmitIR(IR::Cmd::Kind kind) {
  VERIFY_OR_EXIT;
  auto land = IR::Func::Current->AddBlock();

  ASSERT(!key_vals.empty(), "");
  std::vector<IR::Val> phi_args;
  phi_args.reserve(2 * key_vals.size());
  for (size_t i = 0; i < key_vals.size() - 1; ++i) {
    auto compute = IR::Func::Current->AddBlock();
    auto next    = IR::Func::Current->AddBlock();

    auto val = key_vals[i].first->EmitIR(kind);
    IR::CondJump(val, compute, next);

    IR::Block::Current = compute;
    phi_args.push_back(IR::Val::Block(IR::Block::Current));
    auto result = key_vals[i].second->EmitIR(kind);
    phi_args.push_back(result);
    IR::UncondJump(land);

    IR::Block::Current = next;
  }

  // Last entry
  phi_args.push_back(IR::Val::Block(IR::Block::Current));
  auto result = key_vals.back().second->EmitIR(kind);
  phi_args.push_back(result);
  IR::UncondJump(land);

  IR::Block::Current = land;
  auto phi           = IR::Phi(type);
  IR::Func::Current->SetArgs(phi, std::move(phi_args));
  return IR::Func::Current->Command(phi).reg();
}

IR::Val AST::ScopeLiteral::EmitIR(IR::Cmd::Kind) {
  VERIFY_OR_EXIT;
  return IR::Val::Scope(this);
}

IR::Val AST::ScopeNode::EmitIR(IR::Cmd::Kind kind) {
  VERIFY_OR_EXIT;
  IR::Val scope_expr_val = Evaluate(scope_expr.get());
  ASSERT_TYPE(Scope_Type, scope_expr_val.type);

  auto enter_fn = scope_expr_val.value.as<AST::ScopeLiteral *>()
                      ->enter_fn->init_val->EmitIR(kind);
  ASSERT_NE(enter_fn, IR::Val::None());
  auto exit_fn =
      scope_expr_val.value.as<AST::ScopeLiteral *>()->exit_fn->init_val->EmitIR(
          kind);
  ASSERT_NE(exit_fn, IR::Val::None());

  auto call_enter_result =
      IR::Call(enter_fn, expr ? std::vector<IR::Val>{expr->EmitIR(kind)}
                              : std::vector<IR::Val>{});
  auto land_block  = IR::Func::Current->AddBlock();
  auto enter_block = IR::Func::Current->AddBlock();

  IR::CondJump(call_enter_result, enter_block, land_block);

  IR::Block::Current = enter_block;
  stmts->EmitIR(kind);

  auto call_exit_result = IR::Call(exit_fn, {});
  IR::CondJump(call_exit_result, enter_block, land_block);

  IR::Block::Current = land_block;
  return IR::Val::None();
}

IR::Val AST::Declaration::EmitIR(IR::Cmd::Kind kind) {
  VERIFY_OR_EXIT;
  if (addr != IR::Val::None()) { return IR::Val::None(); }

  if (const_) {
    // TODO it's custom or default initialized. cannot be uninitialized. This
    // should be verified by the type system.
    if (IsCustomInitialized()) {
      addr = Evaluate(init_val.get());
    } else if (IsDefaultInitialized()) {
      // TODO if EmitInitialValue requires generating code, that would be bad.
      addr = type->EmitInitialValue();
    } else {
      UNREACHABLE();
    }
  } else if (scope_ == Scope::Global) {
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
  } else {
    // For local variables the declaration determines where the initial value is
    // set, but the allocation has to be done much earlier. We do the allocation
    // in FunctionLiteral::EmitIR. Declaration::EmitIR is just used to set the
    // value.

    // TODO I don't think this is even callable
    ASSERT_NE(addr, IR::Val::None());
    ASSERT(scope_->ContainingFnScope(), "");

    // TODO these checks actually overlap and could be simplified.
    if (IsUninitialized()) { return IR::Val::None(); }

    if (IsCustomInitialized()) {
      lrvalue_check();
      if (init_val->lvalue == Assign::RVal) {
        Type::EmitMoveInit(init_val->type, type, init_val->EmitIR(kind), addr);
      } else {
        Type::EmitCopyInit(init_val->type, type, init_val->EmitIR(kind), addr);
      }
    } else {
      type->EmitInit(addr);
    }
  }
  return IR::Val::None();
}

IR::Val AST::Unop::EmitIR(IR::Cmd::Kind kind) {
  VERIFY_OR_EXIT;

  switch (op) {
  case Language::Operator::Not:
  case Language::Operator::Sub: {
    return IR::Neg(operand->EmitIR(kind));
  } break;
  case Language::Operator::Return: {
    if (operand->is<AST::CommaList>()) {
      bool all_types = true;
      std::vector<IR::Val> vals;
      for (const auto &expr : ptr_cast<AST::CommaList>(operand.get())->exprs) {
        vals.push_back(expr->EmitIR(kind));
        if (expr->type != Type_) { all_types = false; }
      }

      if (all_types) {
        std::vector<Type *> types;
        for (const auto &val : vals) {
          types.push_back(val.value.as<::Type *>());
        }
        IR::SetReturn(IR::ReturnValue{0}, IR::Val::Type(Tup(types)));
        IR::ReturnJump();
      } else {
        size_t i = 0;
        for (auto &val : vals) {
          IR::SetReturn(IR::ReturnValue{static_cast<i32>(i++)}, std::move(val));
        }
        IR::ReturnJump();
      }
    } else {
      auto val = operand->EmitIR(kind);
      if (ErrorLog::NumErrors() != 0) { return IR::Val::None(); }
      IR::SetReturn(IR::ReturnValue{0}, val);
      IR::ReturnJump();
    }

    return IR::Val::None();
  }
  case Language::Operator::Print: {
    ForEachExpr(operand.get(), [kind](size_t, AST::Expression *expr) {
      if (expr->type->is<Primitive>() || expr->type->is<Pointer>()) {
        IR::Print(expr->EmitIR(kind));
      } else {
        expr->type->EmitRepr(expr->EmitIR(kind));
      }
    });

    return IR::Val::None();
  } break;
  case Language::Operator::And: return operand->EmitLVal(kind);
  case Language::Operator::Eval:
    // TODO what if there's an error during evaluation?
    return Evaluate(operand.get());
  case Language::Operator::Generate: {
    auto val = Evaluate(operand.get());
    ASSERT_EQ(val.type, Code);
    val.value.as<AST::CodeBlock *>()->stmts->assign_scope(scope_);
    val.value.as<AST::CodeBlock *>()->stmts->EmitIR(kind);
    return IR::Val::None();
  } break;
  case Language::Operator::Mul: return IR::Ptr(operand->EmitIR(kind));
  case Language::Operator::At: return PtrCallFix(operand->EmitIR(kind));
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
  default: UNREACHABLE("Operator is ", static_cast<int>(op));
  }
}

IR::Val AST::Binop::EmitIR(IR::Cmd::Kind kind) {
  VERIFY_OR_EXIT;
  switch (op) {
#define CASE(op_name)                                                          \
  case Language::Operator::op_name: {                                          \
    auto lhs_ir = lhs->EmitIR(kind);                                           \
    auto rhs_ir = rhs->EmitIR(kind);                                           \
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
    auto lhs_ir = lhs->EmitIR(kind);
    auto rhs_ir = rhs->EmitIR(kind);
    return IR::Cast(lhs_ir, rhs_ir);
  } break;
  case Language::Operator::Call: {
    auto lhs_ir = lhs->EmitIR(kind);
    std::vector<IR::Val> args;
    args.reserve(rhs->as<CallArgs>().bindings_.size());
    for (size_t i = 0; i < rhs->as<CallArgs>().bindings_.size(); ++i) {
      auto *expr = rhs->as<CallArgs>().bindings_[i];
      if (expr == nullptr /* Default */) {
        // TODO what if it's not a function but something else callable (a
        // type-cast)??
        args.push_back(
            lhs_ir.value.as<IR::Func *>()->args_[i].second->EmitIR(kind));
      } else {
        args.push_back(expr->EmitIR(kind));
      }
    }
    return IR::Call(lhs_ir, std::move(args));
  } break;
  case Language::Operator::Assign: {
    std::vector<Type *> lhs_types, rhs_types;
    std::vector<IR::Val> rhs_vals;
    ForEachExpr(rhs.get(),
                [&rhs_vals, &rhs_types, kind](size_t, AST::Expression *expr) {
                  rhs_vals.push_back(expr->EmitIR(kind));
                  rhs_types.push_back(expr->type);
                });

    std::vector<IR::Val> lhs_lvals;
    ForEachExpr(lhs.get(),
                [&lhs_lvals, &lhs_types, kind](size_t, AST::Expression *expr) {
                  lhs_lvals.push_back(expr->EmitLVal(kind));
                  lhs_types.push_back(expr->type);
                });

    ASSERT_EQ(lhs_lvals.size(), rhs_vals.size());
    for (size_t i = 0; i < lhs_lvals.size(); ++i) {
      Type::CallAssignment(rhs_types[i], lhs_types[i], rhs_vals[i],
                           lhs_lvals[i]);
    }
    return IR::Val::None();
  } break;
  case Language::Operator::OrEq: {
    auto land_block = IR::Func::Current->AddBlock();
    auto more_block = IR::Func::Current->AddBlock();

    auto lhs_val       = lhs->EmitIR(kind);
    auto lhs_end_block = IR::Block::Current;
    IR::CondJump(lhs_val, land_block, more_block);

    IR::Block::Current = more_block;
    auto rhs_val       = rhs->EmitIR(kind);
    auto rhs_end_block = IR::Block::Current;
    IR::UncondJump(land_block);

    IR::Block::Current = land_block;

    auto phi = IR::Phi(Bool);
    IR::Func::Current->SetArgs(phi, {IR::Val::Block(lhs_end_block),
                                     IR::Val::Bool(true),
                                     IR::Val::Block(rhs_end_block), rhs_val});
    return IR::Func::Current->Command(phi).reg();
  } break;
  case Language::Operator::AndEq: {
    auto land_block = IR::Func::Current->AddBlock();
    auto more_block = IR::Func::Current->AddBlock();

    auto lhs_val       = lhs->EmitIR(kind);
    auto lhs_end_block = IR::Block::Current;
    IR::CondJump(lhs_val, more_block, land_block);

    IR::Block::Current = more_block;
    auto rhs_val       = rhs->EmitIR(kind);
    auto rhs_end_block = IR::Block::Current;
    IR::UncondJump(land_block);

    IR::Block::Current = land_block;

    auto phi = IR::Phi(Bool);
    IR::Func::Current->SetArgs(phi, {IR::Val::Block(lhs_end_block),
                                     IR::Val::Bool(false),
                                     IR::Val::Block(rhs_end_block), rhs_val});
    return IR::Func::Current->Command(phi).reg();
  } break;
#define CASE_ASSIGN_EQ(op_name)                                                \
  case Language::Operator::op_name##Eq: {                                      \
    auto lhs_lval = lhs->EmitLVal(kind);                                       \
    auto rhs_ir   = rhs->EmitIR(kind);                                         \
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
  case Language::Operator::Index: return PtrCallFix(EmitLVal(kind));
  default: UNREACHABLE(*this);
  }
}

IR::Val AST::ArrayType::EmitIR(IR::Cmd::Kind kind) {
  VERIFY_OR_EXIT;
  return IR::Array(length->EmitIR(kind), data_type->EmitIR(kind));
}

static IR::Val EmitChainOpPair(Type *lhs_type, const IR::Val &lhs_ir,
                               Language::Operator op, Type *rhs_type,
                               const IR::Val &rhs_ir) {
  if (lhs_type->is<Array>() && rhs_type->is<Array>()) {
    ASSERT(op == Language::Operator::Eq || op == Language::Operator::Ne, "");
    return Array::Compare(&lhs_type->as<Array>(), lhs_ir,
                          &rhs_type->as<Array>(), rhs_ir,
                          op == Language::Operator::Eq);
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

IR::Val AST::ChainOp::EmitIR(IR::Cmd::Kind kind) {
  VERIFY_OR_EXIT;
  if (ops[0] == Language::Operator::Xor) {
    auto val = IR::Val::Bool(false);
    for (const auto &expr : exprs) { val = IR::Xor(val, expr->EmitIR(kind)); }
    return val;
  } else if (ops[0] == Language::Operator::Or && type == Type_) {
    // TODO probably want to check that each expression is a type? What if I
    // overload | to take my own stuff and have it return a type?
    std::vector<IR::Val> args;
    args.reserve(exprs.size());
    for (const auto &expr : exprs) { args.push_back(expr->EmitIR(kind)); }
    return IR::Variant(std::move(args));
  } else if (ops[0] == Language::Operator::And ||
             ops[0] == Language::Operator::Or) {
    auto land_block = IR::Func::Current->AddBlock();
    std::vector<IR::Val> phi_args;
    phi_args.reserve(2 * exprs.size());
    bool is_or = (ops[0] == Language::Operator::Or);
    for (size_t i = 0; i < exprs.size() - 1; ++i) {
      auto val = exprs[i]->EmitIR(kind);

      auto next_block = IR::Func::Current->AddBlock();
      IR::CondJump(val, is_or ? land_block : next_block,
                   is_or ? next_block : land_block);
      phi_args.push_back(IR::Val::Block(IR::Block::Current));
      phi_args.push_back(IR::Val::Bool(is_or));

      IR::Block::Current = next_block;
    }

    phi_args.push_back(IR::Val::Block(IR::Block::Current));
    phi_args.push_back(exprs.back()->EmitIR(kind));
    IR::UncondJump(land_block);

    IR::Block::Current = land_block;
    auto phi           = IR::Phi(Bool);
    IR::Func::Current->SetArgs(phi, std::move(phi_args));
    return IR::Func::Current->Command(phi).reg();

  } else {
    if (ops.size() == 1) {
      auto lhs_ir = exprs[0]->EmitIR(kind);
      auto rhs_ir = exprs[1]->EmitIR(kind);
      return EmitChainOpPair(exprs[0]->type, lhs_ir, ops[0], exprs[1]->type,
                             rhs_ir);

    } else {
      std::vector<IR::Val> phi_args;
      auto lhs_ir     = exprs.front()->EmitIR(kind);
      auto land_block = IR::Func::Current->AddBlock();
      for (size_t i = 0; i < ops.size() - 1; ++i) {
        auto rhs_ir = exprs[i + 1]->EmitIR(kind);
        IR::Val cmp = EmitChainOpPair(exprs[i]->type, lhs_ir, ops[i],
                                      exprs[i + 1]->type, rhs_ir);

        phi_args.push_back(IR::Val::Block(IR::Block::Current));
        phi_args.push_back(IR::Val::Bool(false));
        auto next_block = IR::Func::Current->AddBlock();
        IR::CondJump(cmp, next_block, land_block);
        IR::Block::Current = next_block;
        lhs_ir             = rhs_ir;
      }

      // Once more for the last element, but don't do a conditional jump.
      auto rhs_ir = exprs.back()->EmitIR(kind);
      auto last_cmp =
          EmitChainOpPair(exprs[exprs.size() - 2]->type, lhs_ir, ops.back(),
                          exprs[exprs.size() - 1]->type, rhs_ir);
      phi_args.push_back(IR::Val::Block(IR::Block::Current));
      phi_args.push_back(last_cmp);
      IR::UncondJump(land_block);

      IR::Block::Current = land_block;
      auto phi           = IR::Phi(Bool);
      IR::Func::Current->SetArgs(phi, std::move(phi_args));
      return IR::Func::Current->Command(phi).reg();
    }
  }
}

IR::Val AST::CommaList::EmitIR(IR::Cmd::Kind) { UNREACHABLE(); }
IR::Val AST::CommaList::EmitLVal(IR::Cmd::Kind) { NOT_YET(); }

IR::Val AST::FunctionLiteral::EmitTemporaryIR(IR::Cmd::Kind kind) {
  return EmitIRAndSave(false, kind);
}
IR::Val AST::FunctionLiteral::EmitIR(IR::Cmd::Kind) {
  return EmitIRAndSave(true, IR::Cmd::Kind::Exec);
}

IR::Val AST::FunctionLiteral::EmitIRAndSave(bool should_save,
                                            IR::Cmd::Kind kind) {
  VERIFY_OR_EXIT;
  // Verifying 'this' only verifies the declared functions type not the
  // internals. We need to do that here.
  statements->verify_types();
  if (ErrorLog::NumErrors() != 0) { return IR::Val::None(); }

  if (!ir_func) {
    std::vector<std::pair<std::string, AST::Expression *>> args;
    args.reserve(inputs.size());
    for (const auto &input : inputs) {
      args.emplace_back(input->as<Declaration>().identifier->token,
                        input->as<Declaration>().init_val.get());
    }

    if (should_save) {
      IR::Func::All.push_back(
          std::make_unique<IR::Func>(&type->as<Function>(), std::move(args)));
      ir_func = IR::Func::All.back().get();
    } else {
      // TODO XXX This is SUPER DANGEROUS! Depending on a bool passed in we
      // either own or don't own?!?!?!
      ir_func = new IR::Func(&type->as<Function>(), std::move(args));
    }

    CURRENT_FUNC(ir_func) {
      IR::Block::Current = ir_func->entry();

      for (size_t i = 0; i < inputs.size(); ++i) {
        auto &arg = inputs[i];
        ASSERT_EQ(arg->addr, IR::Val::None());
        // TODO This whole loop can be done on construction!
        arg->addr = IR::Func::Current->Argument(static_cast<i32>(i));
      }

      // TODO multiple return types
      if (return_type_expr->is<Declaration>()) {
        return_type_expr->as<Declaration>().addr =
            IR::Val::Ret(0, return_type_expr->type);
      }

      for (auto scope : fn_scope->innards_) {
        scope->ForEachDeclHere([kind, scope](Declaration *decl) {
          // TODO arg_val seems to go along with in_decl a lot. Is there some
          // reason for this that *should* be abstracted?
          if (decl->arg_val || decl->is<InDecl>()) { return; }
          ASSERT(decl->type, "");
          decl->addr = IR::Alloca(decl->type);
          if (decl->init_val) {
            Type::CallAssignment(decl->type, decl->type,
                                 decl->init_val->EmitIR(kind), decl->addr);

          } else {
            decl->type->EmitInit(decl->addr);
          }
        });
      }

      statements->EmitIR(kind);
      IR::ReturnJump();
    }
  }

  return IR::Val::Func(ir_func);
}

IR::Val AST::Statements::EmitIR(IR::Cmd::Kind kind) {
  VERIFY_OR_EXIT;
  for (auto &stmt : statements) { stmt->EmitIR(kind); }
  return IR::Val::None();
}

IR::Val AST::CodeBlock::EmitIR(IR::Cmd::Kind) {
  VERIFY_OR_EXIT;
  std::vector<IR::Val> args;
  stmts->contextualize(scope_, &args);
  return IR::Contextualize(this, std::move(args));
}

IR::Val AST::Identifier::EmitLVal(IR::Cmd::Kind kind) {
  VERIFY_OR_EXIT;
  ASSERT(decl != nullptr, "");

  if (decl->addr == IR::Val::None()) { decl->EmitIR(kind); }
  // TODO kind???
  return decl->addr;
}

IR::Val AST::Unop::EmitLVal(IR::Cmd::Kind kind) {
  switch (op) {
  case Language::Operator::At: return operand->EmitIR(kind);
  default: UNREACHABLE("Operator is ", static_cast<int>(op));
  }
}

IR::Val AST::Binop::EmitLVal(IR::Cmd::Kind kind) {
  switch (op) {
  case Language::Operator::Index:
    if (lhs->type->is<::Array>()) {
      return IR::Index(lhs->EmitLVal(kind), rhs->EmitIR(kind));
    }
  default: UNREACHABLE("Operator is ", static_cast<int>(op));
  }
}
