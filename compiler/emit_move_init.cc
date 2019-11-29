#include "compiler/compiler.h"

#include "ast/ast.h"
#include "ir/cmd/misc.h"
#include "ir/components.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

void Compiler::EmitMoveInit(type::Type const *from_type,
                            ir::Results const &from_val,
                            type::Typed<ir::Reg> to_var) {
  auto *to_type = to_var.type()->as<type::Pointer>().pointee;
  // TODO Optimize once you understand the semantics better.
  if (not to_type->is<type::Primitive>() and
      not to_type->is<type::Function>() and not to_type->is<type::Variant>() and
      not to_type->is<type::Enum>() and not to_type->is<type::Flags>()) {
    Visit(to_type, to_var.get(), EmitDefaultInitTag{});
  }

  Visit(to_type, to_var.get(), type::Typed{from_val, from_type},
        EmitMoveAssignTag{});
}

void Compiler::Visit(ast::Expression const *node, type::Typed<ir::Reg> reg,
                     EmitMoveInitTag) {
  EmitMoveInit(type_of(node), Visit(node, EmitValueTag{}), reg);
}

void Compiler::Visit(ast::ArrayLiteral const *node, type::Typed<ir::Reg> reg,
                     EmitMoveInitTag) {
  type::Array const &array_type = type_of(node)->as<type::Array>();
  auto *data_type_ptr           = type::Ptr(array_type.data_type);
  auto elem = ir::Index(type::Ptr(&array_type), reg.get(), 0);
  for (size_t i = 0; i + 1 < array_type.len; ++i) {
    Visit(node->elem(i), type::Typed<ir::Reg>(elem, data_type_ptr),
          EmitMoveInitTag{});
    elem = builder().PtrIncr(elem, 1, data_type_ptr);
  }
  Visit(node->elems().back(), type::Typed<ir::Reg>(elem, data_type_ptr),
        EmitMoveInitTag{});
}

void Compiler::Visit(ast::CommaList const *node, type::Typed<ir::Reg> reg,
                     EmitMoveInitTag) {
  size_t index  = 0;
  auto const &t = reg.type()->as<type::Pointer>().pointee->as<type::Tuple>();
  for (auto &expr : node->exprs_) {
    if (expr->needs_expansion()) {
      auto results = Visit(expr.get(), EmitValueTag{});
      for (size_t i = 0; i < results.size(); ++i) {
        EmitMoveInit(t.entries_[index], results.GetResult(i),
                     builder().Field(reg.get(), &t, index));
        ++index;
      }
    } else {
      Visit(expr.get(), builder().Field(reg.get(), &t, index),
            EmitMoveInitTag{});
      ++index;
    }
  }
}

void Compiler::Visit(ast::Unop const *node, type::Typed<ir::Reg> reg,
                     EmitMoveInitTag) {
  switch (node->op()) {
    case frontend::Operator::Move:
      Visit(node->operand(), reg, EmitMoveInitTag{});
      break;
    case frontend::Operator::Copy:
      Visit(node->operand(), reg, EmitCopyInitTag{});
      break;
    default:
      EmitMoveInit(type_of(node), Visit(node, EmitValueTag{}), reg);
      break;
  }
}

}  // namespace compiler
