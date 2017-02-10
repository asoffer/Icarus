#include "IR.h"
#include "Type/Type.h"

namespace AST {
IR::Value Identifier::EmitLVal() { return decl->addr; }

IR::Value Binop::EmitLVal() {
  if (op == Language::Operator::Index) {
    if (lhs->type->is_array()) {
      auto array_type = (Array *)lhs->type;

      return IR::Access(
          array_type->data_type, rhs->EmitIR(),
          array_type->fixed_length
              ? lhs->EmitLVal()
              : IR::Load(Ptr(array_type->data_type),
                         IR::ArrayData(array_type, lhs->EmitLVal())));
    } else if (lhs->type == String) {
      return IR::Access(Char, rhs->EmitIR(),
                        IR::Cast(String, Ptr(Char), lhs->EmitIR()));
    } else {
      UNREACHABLE;
    }
  } else {
    NOT_YET;
  }
}


IR::Value Unop::EmitLVal() {
  // TODO EmitIR or EmitLVal?
  if (op == Language::Operator::At) { return operand->EmitIR(); }
  NOT_YET;
}
IR::Value ChainOp::EmitLVal() { NOT_YET; }
IR::Value Generic::EmitLVal() { NOT_YET; }
IR::Value InDecl::EmitLVal() { NOT_YET; }

IR::Value Access::EmitLVal() {
  // Automatically pass through pointers
  auto etype  = operand->type;
  auto e_lval = operand->EmitLVal();
  /*
     assert(etype->is_pointer());
    auto ptee = ((Pointer *)etype)->pointee;
    while (ptee->is_pointer()) {
      e_lval = IR::Load(ptee, e_lval);
      etype  = ptee;
      ptee   = ((Pointer *)ptee)->pointee;
    }

    assert(ptee->is_struct());
    auto struct_type = (Struct *)ptee;
    return IR::Field(struct_type, e_lval,
                     struct_type->field_name_to_num AT(member_name));
  }
   */
  // TODO This might be a hack
  if (e_lval.flag == IR::ValType::Loc && e_lval.as_loc->is_arg()) {
    assert(etype->is_pointer());
    auto ptee = ((Pointer *)etype)->pointee;
    while (ptee->is_pointer()) {
      e_lval = IR::Load(ptee, e_lval);
      etype  = ptee;
      ptee   = ((Pointer *)ptee)->pointee;
    }
    etype = ptee;

  } else {
    while (etype->is_pointer()) {
      e_lval = IR::Load(etype, e_lval);
      etype  = ((Pointer *)etype)->pointee;
    }
  }

  assert(etype->is_struct());
  auto struct_type = (Struct *)etype;
  return IR::Field(struct_type, e_lval,
                   struct_type->field_name_to_num AT(member_name));
}

IR::Value CodeBlock::EmitLVal() { UNREACHABLE; }
IR::Value Terminal::EmitLVal() { UNREACHABLE; }
IR::Value Declaration::EmitLVal() { UNREACHABLE; }
IR::Value FunctionLiteral::EmitLVal() { UNREACHABLE; }
IR::Value ArrayType::EmitLVal() { UNREACHABLE; }
IR::Value Case::EmitLVal() { UNREACHABLE; }
IR::Value ArrayLiteral::EmitLVal() { UNREACHABLE; }
IR::Value ScopeLiteral::EmitLVal() { UNREACHABLE; }

IR::Value DummyTypeExpr::EmitLVal() { UNREACHABLE; }
} // namespace AST
