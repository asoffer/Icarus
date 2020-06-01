#include "compiler/compiler.h"

#include "ast/ast.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

void Compiler::EmitCopyInit(type::Typed<ir::Value> from_val,
                            type::Typed<ir::Reg> to_var) {
  auto *to_type = to_var.type()->as<type::Pointer>().pointee();
  // TODO Optimize once you understand the semantics better.
  if (not to_type->IsDefaultInitializable()) {
    Visit(to_type, to_var.get(), EmitDefaultInitTag{});
  }
  Visit(to_type, to_var.get(), from_val, EmitCopyAssignTag{});
}

void Compiler::EmitCopyInit(ast::Expression const *node,
                            type::Typed<ir::Reg> reg) {
  EmitCopyInit(type::Typed(EmitValue(node), type_of(node)), reg);
}

void Compiler::EmitCopyInit(ast::ArrayLiteral const *node,
                            type::Typed<ir::Reg> reg) {
  type::Array const &array_type = type_of(node)->as<type::Array>();
  auto *data_type_ptr           = type::Ptr(array_type.data_type());
  auto elem = builder().Index(type::Ptr(&array_type), reg.get(), 0);
  for (size_t i = 0; i + 1 < array_type.length(); ++i) {
    EmitCopyInit(node->elem(i), type::Typed<ir::Reg>(elem, data_type_ptr));
    elem = builder().PtrIncr(elem, 1, data_type_ptr);
  }
  EmitCopyInit(node->elems().back(), type::Typed<ir::Reg>(elem, data_type_ptr));
}

}  // namespace compiler
