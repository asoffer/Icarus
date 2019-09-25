#include "compiler/compiler.h"

#include "ast/ast.h"
#include "ir/cmd/misc.h"
#include "ir/components.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

void Compiler::EmitCopyInit(type::Type const *from_type,
                                          ir::Results const &from_val,
                                          type::Typed<ir::Reg> to_var) {
  auto *to_type = to_var.type()->as<type::Pointer>().pointee;
  // TODO Optimize once you understand the semantics better.
  if (!to_type->is<type::Primitive>() && !to_type->is<type::Function>() &&
      !to_type->is<type::Variant>()) {
    to_type->EmitDefaultInit(this, to_var.get());
  }

  to_type->EmitCopyAssign(this, from_type, from_val, to_var.get());
}

void Compiler::EmitCopyInit(ast::Expression const *node,
                                          type::Typed<ir::Reg> reg) {
  EmitCopyInit(type_of(node), node->EmitValue(this), reg);
}

void Compiler::EmitCopyInit(ast::ArrayLiteral const *node,
                                          type::Typed<ir::Reg> reg) {
  type::Array const &array_type = type_of(node)->as<type::Array>();
  auto *data_type_ptr           = type::Ptr(array_type.data_type);
  auto elem = ir::Index(type::Ptr(&array_type), reg.get(), 0);
  for (size_t i = 0; i + 1 < array_type.len; ++i) {
    node->elem(i)->EmitCopyInit(this,
                                type::Typed<ir::Reg>(elem, data_type_ptr));
    elem = ir::PtrIncr(elem, 1, data_type_ptr);
  }
  node->elems().back()->EmitCopyInit(this,
                                     type::Typed<ir::Reg>(elem, data_type_ptr));
}

void Compiler::EmitCopyInit(ast::CommaList const *node,
                                          type::Typed<ir::Reg> reg) {
  size_t index  = 0;
  auto const &t = reg.type()->as<type::Pointer>().pointee->as<type::Tuple>();
  for (auto &expr : node->exprs_) {
    if (expr->needs_expansion()) {
      auto results = expr->EmitValue(this);
      for (size_t i = 0; i < results.size(); ++i) {
        EmitCopyInit(t.entries_[index], results.GetResult(i),
                     ir::Field(reg.get(), &t, index));
        ++index;
      }
    } else {
      expr->EmitCopyInit(this, ir::Field(reg.get(), &t, index));
      ++index;
    }
  }
}

void Compiler::EmitCopyInit(ast::Unop const *node,
                                          type::Typed<ir::Reg> reg) {
  switch (node->op()) {
    case frontend::Operator::Move:
      node->operand()->EmitMoveInit(this, reg);
      break;
    case frontend::Operator::Copy:
      node->operand()->EmitCopyInit(this, reg);
      break;
    default: EmitCopyInit(type_of(node), node->EmitValue(this), reg); break;
  }
}

}  // namespace compiler
