#include "compiler/compiler.h"

#include "ast/ast.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

void Compiler::EmitMoveInit(type::Typed<ir::Value> from_val,
                            type::Typed<ir::Reg> to_var) {
  auto *to_type = to_var.type()->as<type::Pointer>().pointee();
  // TODO Optimize once you understand the semantics better.
  if (to_type->IsDefaultInitializable()) {
    Visit(to_type, to_var.get(), EmitDefaultInitTag{});
  }

  Visit(to_type, to_var.get(), from_val, EmitMoveAssignTag{});
}

void Compiler::EmitMoveInit(ast::Expression const *node,
                            type::Typed<ir::Reg> reg) {
  EmitMoveInit(type::Typed(EmitValue(node), type_of(node)), reg);
}

void Compiler::EmitMoveInit(ast::ArrayLiteral const *node,
                            type::Typed<ir::Reg> reg) {
  type::Array const &array_type = type_of(node)->as<type::Array>();
  auto *data_type_ptr           = type::Ptr(array_type.data_type());
  auto elem = builder().Index(type::Ptr(&array_type), reg.get(), 0);
  for (size_t i = 0; i + 1 < array_type.length(); ++i) {
    EmitMoveInit(node->elem(i), type::Typed<ir::Reg>(elem, data_type_ptr));
    elem = builder().PtrIncr(elem, 1, data_type_ptr);
  }
  EmitMoveInit(node->elems().back(), type::Typed<ir::Reg>(elem, data_type_ptr));
}

void Compiler::EmitMoveInit(ast::UnaryOperator const *node,
                            type::Typed<ir::Reg> reg) {
  switch (node->op()) {
    case frontend::Operator::Move: EmitMoveInit(node->operand(), reg); break;
    case frontend::Operator::Copy: EmitCopyInit(node->operand(), reg); break;
    default:
      EmitMoveInit(type::Typed(EmitValue(node), type_of(node)), reg);
      break;
  }
}

}  // namespace compiler
