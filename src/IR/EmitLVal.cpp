#include "IR.h"
#include "Type/Type.h"

namespace AST {
IR::Value Identifier::EmitLVal() {
  return IR::Value::RelAlloc(IR::Func::Current->frame_map.at(decl));
}

IR::Value Binop::EmitLVal() {
  if (op == Language::Operator::Index && lhs->type->is_array()) {
    auto array_type = (Array*)lhs->type;
    auto lhs_val  = lhs->EmitLVal();
    auto rhs_val  = rhs->EmitIR();
    int index;
    if (rhs_val.flag == IR::ValType::U) {
      index = (int)rhs_val.val.as_uint;
    } else if (rhs_val.flag == IR::ValType::I) {
      index = rhs_val.val.as_int;
    } else {
      assert(false);
    }

    if (array_type->fixed_length) {
      auto gep = IR::GEP(array_type, lhs_val, {0, index});
      IR::Block::Current->push(gep);
      return gep;
    } else {
      NOT_YET;
    }

  } else {
    NOT_YET;
  }
}

IR::Value Unop::EmitLVal() { NOT_YET; }
IR::Value ChainOp::EmitLVal() { NOT_YET; }
IR::Value DummyTypeExpr::EmitLVal() { NOT_YET; }
IR::Value Generic::EmitLVal() { NOT_YET; }
IR::Value InDecl::EmitLVal() { NOT_YET; }
IR::Value Access::EmitLVal() { NOT_YET; }

IR::Value Terminal::EmitLVal() { UNREACHABLE; }
IR::Value Declaration::EmitLVal() { UNREACHABLE; }
IR::Value FunctionLiteral::EmitLVal() { UNREACHABLE; }
IR::Value ArrayType::EmitLVal() { UNREACHABLE; }
IR::Value StructLiteral::EmitLVal() { UNREACHABLE; }
IR::Value ParametricStructLiteral::EmitLVal() { UNREACHABLE; }
IR::Value Case::EmitLVal() { UNREACHABLE; }
IR::Value ArrayLiteral::EmitLVal() { UNREACHABLE; }
IR::Value EnumLiteral::EmitLVal() { UNREACHABLE; }
} // namespace AST
