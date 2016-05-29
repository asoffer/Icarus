#include "IR.h"
#include "Type.h"

#define NOT_YET assert(false && "Not yet implemented")

extern llvm::Value *GetFunctionReferencedIn(Scope *scope,
                                            const std::string &fn_name,
                                            Type *input_type);

namespace AST {
IR::Value Terminal::EmitIR() const {
  switch (terminal_type) {
  case Language::Terminal::ASCII: NOT_YET;
  case Language::Terminal::Char: NOT_YET;
  case Language::Terminal::Else: return IR::Value(true);
  case Language::Terminal::False: return IR::Value(false);
  case Language::Terminal::Hole: NOT_YET;
  case Language::Terminal::Int: NOT_YET;
  case Language::Terminal::Null: NOT_YET;
  case Language::Terminal::Ord: NOT_YET;
  case Language::Terminal::Real: NOT_YET;
  case Language::Terminal::Return: NOT_YET;
  case Language::Terminal::StringLiteral: NOT_YET;
  case Language::Terminal::True: return IR::Value(true);
  case Language::Terminal::Type: NOT_YET; 
  case Language::Terminal::Uint: NOT_YET; 
  }
}

IR::Value Unop::EmitIR() const {
  switch (op) {
  case Language::Operator::Import: NOT_YET;
  case Language::Operator::Return: NOT_YET;
  case Language::Operator::Break: NOT_YET;
  case Language::Operator::Continue: NOT_YET;
  case Language::Operator::Repeat: NOT_YET;
  case Language::Operator::Restart: NOT_YET;
  case Language::Operator::Free: NOT_YET;
  case Language::Operator::Print: NOT_YET;
  case Language::Operator::And: NOT_YET;
  case Language::Operator::Sub: {
    auto val = operand->EmitIR();
    if (operand->type == Int) {
      return IR::INeg(val);

    } else if (operand->type == Real) {
      return IR::FNeg(val);

    } else {
      auto neg_fn = GetFunctionReferencedIn(scope_, "__neg__", operand->type);
      assert(neg_fn && "No 'not' function available.");
      NOT_YET; // return IR::Call(neg_fn, {val});
    }
  } break;
  case Language::Operator::Not: {
    auto val = operand->EmitIR();
    if (operand->type == Bool) {
      return BNot(val);

    } else {
      auto not_fn = GetFunctionReferencedIn(scope_, "__not__", operand->type);
      assert(not_fn && "No 'not' function available.");
      NOT_YET; // return IR::Call(not_fn, {val});
    }
  } break;
  case Language::Operator::At: {
    return IR::Load(operand->EmitIR());
  } break;
  case Language::Operator::Call: {
                                   NOT_YET;
    // return IRB.AddCall(neg_fn->ir);
  } break;
  default: NOT_YET;
  }
}

IR::Value Binop::EmitIR() const {
  switch (op) {
  case Language::Operator::Assign: NOT_YET;
  case Language::Operator::Cast: NOT_YET;
  case Language::Operator::OrEq: NOT_YET;
  case Language::Operator::XorEq: NOT_YET;
  case Language::Operator::AndEq: NOT_YET;
  case Language::Operator::AddEq: {
    auto lval    = lhs->EmitIR(); //EmitLValue();
    auto lhs_val = IR::Load(lval);
    auto rhs_val = rhs->EmitIR();

    if (lhs->type == Int && rhs->type == Int) {
      return IR::Store(IR::IAdd(lhs_val, rhs_val), lval);

    } else if (lhs->type == Uint && rhs->type == Uint) {
      return IR::Store(IR::UAdd(lhs_val, rhs_val), lval);

    } else if (lhs->type == Real && rhs->type == Real) {
      return IR::Store(IR::FAdd(lhs_val, rhs_val), lval);

    } else {
      auto add_eq_fn = GetFunctionReferencedIn(scope_, "__add_eq__",
                                               Tup({lhs->type, rhs->type}));
      assert(add_eq_fn && "No 'add_eq' function available");
      NOT_YET; // return IR::Call(add_eq_fn, {lhs_val, rhs_val});
    }
  } break;
  case Language::Operator::SubEq: {
    auto lval    = lhs->EmitIR(); //EmitLValue();
    auto lhs_val = IR::Load(lval);
    auto rhs_val = rhs->EmitIR();

    if (lhs->type == Int && rhs->type == Int) {
      return IR::Store(IR::ISub(lhs_val, rhs_val), lval);

    } else if (lhs->type == Uint && rhs->type == Uint) {
      return IR::Store(IR::USub(lhs_val, rhs_val), lval);

    } else if (lhs->type == Real && rhs->type == Real) {
      return IR::Store(IR::FSub(lhs_val, rhs_val), lval);

    } else {
      auto sub_eq_fn = GetFunctionReferencedIn(scope_, "__sub_eq__",
                                               Tup({lhs->type, rhs->type}));
      assert(sub_eq_fn && "No 'sub_eq' function available");
      NOT_YET; // return IR::Call(sub_eq_fn, {lhs_val, rhs_val});
    }
  } break;
  case Language::Operator::MulEq: {
    auto lval    = lhs->EmitIR(); // EmitLValue();
    auto lhs_val = IR::Load(lval);
    auto rhs_val = rhs->EmitIR();

    if (lhs->type == Int && rhs->type == Int) {
      return IR::Store(IR::IMul(lhs_val, rhs_val), lval);

    } else if (lhs->type == Uint && rhs->type == Uint) {
      return IR::Store(IR::UMul(lhs_val, rhs_val), lval);

    } else if (lhs->type == Real && rhs->type == Real) {
      return IR::Store(IR::FMul(lhs_val, rhs_val), lval);

    } else {
      auto mul_eq_fn = GetFunctionReferencedIn(scope_, "__mul_eq__",
                                               Tup({lhs->type, rhs->type}));
      assert(mul_eq_fn && "No 'mul_eq' function available");
      NOT_YET; // return IR::Call(mul_eq_fn, {lhs_val, rhs_val});
    }
  } break;
  case Language::Operator::DivEq: {
    auto lval    = lhs->EmitIR(); //EmitLValue();
    auto lhs_val = IR::Load(lval);
    auto rhs_val = rhs->EmitIR();

    if (lhs->type == Int && rhs->type == Int) {
      return IR::Store(IR::IDiv(lhs_val, rhs_val), lval);

    } else if (lhs->type == Uint && rhs->type == Uint) {
      return IR::Store(IR::UDiv(lhs_val, rhs_val), lval);

    } else if (lhs->type == Real && rhs->type == Real) {
      return IR::Store(IR::FDiv(lhs_val, rhs_val), lval);

    } else {
      auto div_eq_fn = GetFunctionReferencedIn(scope_, "__div_eq__",
                                               Tup({lhs->type, rhs->type}));
      assert(div_eq_fn && "No 'div_eq' function available");
      NOT_YET; // return IR::Call(div_eq_fn, {lhs_val, rhs_val});
    }
  } break;
  case Language::Operator::ModEq: {
    auto lval    = lhs->EmitIR(); //EmitLValue();
    auto lhs_val = IR::Load(lval);
    auto rhs_val = rhs->EmitIR();

    if (lhs->type == Int && rhs->type == Int) {
      return IR::Store(IR::IMod(lhs_val, rhs_val), lval);

    } else if (lhs->type == Uint && rhs->type == Uint) {
      return IR::Store(IR::UMod(lhs_val, rhs_val), lval);

    } else if (lhs->type == Real && rhs->type == Real) {
      return IR::Store(IR::FMod(lhs_val, rhs_val), lval);

    } else {
      auto mod_eq_fn = GetFunctionReferencedIn(scope_, "__mod_eq__",
                                               Tup({lhs->type, rhs->type}));
      assert(mod_eq_fn && "No 'mod_eq' function available");
      NOT_YET; // return IR::Call(mod_eq_fn, {lhs_val, rhs_val});
    }
  } break;

  default: NOT_YET;
  }
}

IR::Value ChainOp::EmitIR() const { NOT_YET; }
IR::Value Identifier::EmitIR() const { NOT_YET; }
IR::Value Declaration::EmitIR() const { NOT_YET; }
IR::Value Generic::EmitIR() const { NOT_YET; }
IR::Value InDecl::EmitIR() const { NOT_YET; }
IR::Value ParametricStructLiteral::EmitIR() const { NOT_YET; }
IR::Value StructLiteral::EmitIR() const { NOT_YET; }
IR::Value Case::EmitIR() const { NOT_YET; }
IR::Value Access::EmitIR() const { NOT_YET; }
IR::Value ArrayLiteral::EmitIR() const { NOT_YET; }
IR::Value FunctionLiteral::EmitIR() const { NOT_YET; }
IR::Value EnumLiteral::EmitIR() const { NOT_YET; }
IR::Value DummyTypeExpr::EmitIR() const { NOT_YET; }
IR::Value ArrayType::EmitIR() const { NOT_YET; }
} // namespace AST

#undef NOT_YET
