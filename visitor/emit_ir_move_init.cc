#include "visitor/emit_ir.h"

#include "ast/ast.h"
#include "ir/cmd.h"
#include "ir/cmd/misc.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace visitor {

void EmitIr::MoveInit(type::Type const *from_type, ir::Results const &from_val,
                      type::Typed<ir::Reg> to_var, Context *ctx) {
  auto *to_type = to_var.type()->as<type::Pointer>().pointee;
  // TODO Optimize once you understand the semantics better.
  if (!to_type->is<type::Primitive>() && !to_type->is<type::Function>() &&
      !to_type->is<type::Variant>() && !to_type->is<type::Enum>() &&
      !to_type->is<type::Flags>()) {
    to_type->EmitDefaultInit(this, to_var.get(), ctx);
  }

  to_type->EmitMoveAssign(this, from_type, from_val, to_var.get(), ctx);
}

void EmitIr::MoveInit(ast::Expression const *node, type::Typed<ir::Reg> reg,
                      Context *ctx) {
  MoveInit(ctx->type_of(node), node->EmitIr(this, ctx), reg, ctx);
}

void EmitIr::MoveInit(ast::ArrayLiteral const *node, type::Typed<ir::Reg> reg,
                      Context *ctx) {
  type::Array const &array_type = ctx->type_of(node)->as<type::Array>();
  auto *data_type_ptr           = type::Ptr(array_type.data_type);
  auto elem = ir::Index(type::Ptr(&array_type), reg.get(), 0);
  for (size_t i = 0; i + 1 < array_type.len; ++i) {
    node->elem(i)->EmitMoveInit(this, type::Typed<ir::Reg>(elem, data_type_ptr),
                                ctx);
    elem = ir::PtrIncr(elem, 1, data_type_ptr);
  }
  node->elems().back()->EmitMoveInit(
      this, type::Typed<ir::Reg>(elem, data_type_ptr), ctx);
}

void EmitIr::MoveInit(ast::CommaList const *node, type::Typed<ir::Reg> reg,
                      Context *ctx) {
  size_t index  = 0;
  auto const &t = reg.type()->as<type::Pointer>().pointee->as<type::Tuple>();
  for (auto &expr : node->exprs_) {
    if (expr->needs_expansion()) {
      auto results = expr->EmitIr(this, ctx);
      for (size_t i = 0; i < results.size(); ++i) {
        MoveInit(t.entries_[index], results.GetResult(i),
                 ir::Field(reg.get(), &t, index), ctx);
        ++index;
      }
    } else {
      expr->EmitMoveInit(this, ir::Field(reg.get(), &t, index), ctx);
      ++index;
    }
  }
}

void EmitIr::MoveInit(ast::Unop const *node, type::Typed<ir::Reg> reg,
                      Context *ctx) {
  switch (node->op) {
    case frontend::Operator::Move:
      node->operand->EmitMoveInit(this, reg, ctx);
      break;
    case frontend::Operator::Copy:
      node->operand->EmitCopyInit(this, reg, ctx);
      break;
    default:
      MoveInit(ctx->type_of(node), node->EmitIr(this, ctx), reg, ctx);
      break;
  }
}

}  // namespace visitor
