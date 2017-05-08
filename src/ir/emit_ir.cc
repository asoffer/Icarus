#include "ir.h"

#include "../ast/ast.h"
#include "../scope.h"
#include "../type/type.h"

#define VERIFY_OR_EXIT_EARLY                                                   \
  do {                                                                         \
    verify_types(errors);                                                      \
    if (!errors->empty()) { return IR::Val::None(); }                          \
  } while (false)

extern IR::Val PtrCallFix(IR::Val v);

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

IR::Val AST::Terminal::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT_EARLY;

  switch (terminal_type) {
  case Language::Terminal::Char:
  case Language::Terminal::Int:
  case Language::Terminal::Real:
  case Language::Terminal::Type:
  case Language::Terminal::Uint:
  case Language::Terminal::True:
  case Language::Terminal::False:
    return value;
  case Language::Terminal::ASCII:
    return AsciiFunc();
  case Language::Terminal::Ord:
    return OrdFunc();
  case Language::Terminal::Return:
    IR::Jump::Return();
    return IR::Val::None();
  default:
    NOT_YET;
  }
}

IR::Val AST::Identifier::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT_EARLY;
  ASSERT(decl, "");
  if (decl->arg_val) {
    return decl->addr;
  } else {
    return PtrCallFix(EmitLVal(errors));
  }
}

IR::Val AST::Unop::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT_EARLY;
  switch (op) {
  case Language::Operator::Not:
  case Language::Operator::Sub:{
    return IR::Neg(operand->EmitIR(errors));
  } break;
  case Language::Operator::Return: {
    IR::SetReturn(0, operand->EmitIR(errors));

    ASSERT(scope_->is_exec(), "");
    // static_cast<BlockScope *>(scope_)->MakeReturn(operand->EmitIR(errors));
    IR::Jump::Unconditional(IR::BlockIndex{1});

    // TODO this is the right number but not implemented correctly.
    IR::Block::Current = IR::BlockIndex{1};
    IR::Jump::Return();
    return IR::Val::None();
  }
  case Language::Operator::Print: {
    return IR::Print(operand->EmitIR(errors));
  } break;
  default: { UNREACHABLE; }
  }
}

IR::Val AST::Binop::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT_EARLY;
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
    if (rhs->is_comma_list()) {
      NOT_YET;
    } else {
      args.push_back(rhs->EmitIR(errors));
    }
    return IR::Call(lhs_ir, std::move(args));
  } break;
  case Language::Operator::Assign: {
    auto lhs_lval = lhs->EmitLVal(errors);
    auto rhs_ir = rhs->EmitIR(errors);
    return IR::Store(rhs_ir, lhs_lval);
  } break;
  case Language::Operator::OrEq: {
    NOT_YET;
  } break;
  case Language::Operator::AndEq: {
    NOT_YET;
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
  VERIFY_OR_EXIT_EARLY;
  return IR::Array(length->EmitIR(errors), data_type->EmitIR(errors));
}

IR::Val AST::ChainOp::EmitIR(std::vector<Error> *errors) {
  VERIFY_OR_EXIT_EARLY;
  if (ops[0] == Language::Operator::Xor) {
    return std::accumulate(exprs.begin(), exprs.end(), IR::Val::Bool(false),
                           [errors](IR::Val lhs, AST::Expression *expr) {
                             return IR::Xor(lhs, expr->EmitIR(errors));
                           });
  } else {
    std::vector<IR::BlockIndex> blocks;
    blocks.reserve(exprs.size());

    for (size_t i = 0; i < exprs.size(); ++i) {
      blocks.push_back(IR::Func::Current->AddBlock());
    }
    auto land_block = blocks.back();

    auto lhs_ir = exprs[0]->EmitIR(errors);
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
        default:
          std::cerr << *this << std::endl;
          UNREACHABLE;
      }
      IR::Jump::Conditional(cmp, blocks[i], land_block);
      IR::Block::Current = blocks[i];
      lhs_ir = rhs_ir;
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
    // TODO FIXME XXX THIS IS HACKY!
    IR::Func::Current->blocks_[phi.as_reg.block_index.value]
        .cmds_[phi.as_reg.instr_index]
        .args = phi_args;

    return phi;
  }

  NOT_YET;
}

IR::Val AST::FunctionLiteral::Emit(bool, std::vector<Error> *errors) {
  VERIFY_OR_EXIT_EARLY;

  CURRENT_FUNC(ir_func = new IR::Func(type)) {
    IR::Block::Current = IR::BlockIndex{0};
    for (size_t i = 0; i < inputs.size(); ++i) {
      auto arg = inputs[i];
      ASSERT(arg->addr == IR::Val::None(), "");
      arg->addr = IR::Val::Arg(
          arg->type, i); // This whole loop can be done on construction!
    }

    statements->EmitIR(errors);
  }

  return IR::Val::Func(ir_func);
}

IR::Val AST::Statements::EmitIR(std::vector<Error> *errors) {
  for (auto stmt : statements) { stmt->EmitIR(errors); }
  return IR::Val::None();
}

IR::Val AST::Identifier::EmitLVal(std::vector<Error> *errors) {
  VERIFY_OR_EXIT_EARLY;
  ASSERT(decl, "");
  ASSERT(decl->addr != IR::Val::None(), decl->to_string(0));
  return decl->addr;
}
