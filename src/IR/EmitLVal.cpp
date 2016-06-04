#include "IR.h"

namespace AST {
IR::Value Identifier::EmitLVal() {
  return IR::Value::RelAlloc(IR::Func::Current->frame_map.at(this));
}

IR::Value Unop::EmitLVal() { NOT_YET; }
IR::Value Binop::EmitLVal() { NOT_YET; }
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
