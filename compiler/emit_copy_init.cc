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

void Compiler::EmitCopyInit(
    ast::Expression const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to_vars) {
  EmitCopyInit(type::Typed(EmitValue(node), type_of(node)),
               type::Typed(to_vars[0]->reg(), to_vars[0].type()));
}

void Compiler::EmitCopyInit(
    ast::ArrayLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::Addr>> const> to_vars) {
  ASSERT(to_vars.size() == 1u);
  type::Array const &array_type =
      ASSERT_NOT_NULL(data().qual_type(node))->type()->as<type::Array>();
  auto *data_type_ptr           = type::Ptr(array_type.data_type());
  auto elem = builder().Index(type::Ptr(&array_type), to_vars[0]->reg(), 0);
  // Skip the last entry so we don't increment past the end of the array.
  for (size_t i = 0; i + 1 < array_type.length(); ++i) {
    type::Typed<ir::RegOr<ir::Addr>> to_var(elem, data_type_ptr);
    EmitCopyInit(node->elems().back(), absl::MakeConstSpan(&to_var, 1));
    elem = builder().PtrIncr(elem, 1, data_type_ptr);
  }
  type::Typed<ir::RegOr<ir::Addr>> to_var(elem, data_type_ptr);
  EmitCopyInit(node->elems().back(), absl::MakeConstSpan(&to_var, 1));
}

}  // namespace compiler
