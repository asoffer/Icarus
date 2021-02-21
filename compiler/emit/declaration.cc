#include <vector>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "ir/value/addr.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "ir/value/value.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {
namespace {

ir::Value EmitConstantDeclaration(Compiler &c, ast::Declaration const *node) {
  LOG("EmitConstantDeclaration", "%s", node->DebugString());
  if (node->flags() & ast::Declaration::f_IsFnParam) {
    // TODO: Support multiple declarations.
    auto val = c.context().LoadConstantParam(&node->ids()[0]);
    LOG("Declaration", "%s", val);
    return val;
  } else {
    // TODO: Support multiple declarations.
    if (auto *constant_value = c.context().Constant(&node->ids()[0])) {
      // TODO: This feels quite hacky.
      if (node->init_val()->is<ast::StructLiteral>()) {
        if (not constant_value->complete and c.state().must_complete) {
          LOG("compile-work-queue", "Request work complete-struct: %p", node);
          c.Enqueue({
              .kind      = WorkItem::Kind::CompleteStructMembers,
              .node      = node->init_val(),
              .resources = c.resources(),
          });
        }
      }
      return constant_value->value();
    }

    // TODO: Support multiple declarations
    auto qts = c.context().qual_types(&node->ids()[0]);
    auto t   = qts[0].type();

    if (auto const *init_val = node->initial_value()) {
      LOG("Declaration", "Computing slot with %s",
          node->initial_value()->DebugString());

      if (t.is_big()) {
        auto value_buffer = c.EvaluateToBufferOrDiagnose(
            type::Typed<ast::Expression const *>(node->initial_value(), t));
        if (value_buffer.empty()) { return ir::Value(); }

        LOG("EmitConstatnDeclaration", "Setting slot = %s", value_buffer);
        // TODO: Support multiple declarations
        return c.context().SetConstant(&node->ids()[0], std::move(value_buffer));
      } else {
        auto maybe_val = c.Evaluate(
            type::Typed<ast::Expression const *>(node->initial_value(), t),
            c.state().must_complete);
        if (not maybe_val) {
          // TODO: we reserved a slot and haven't cleaned it up. Do we care?
          c.diag().Consume(maybe_val.error());
          return ir::Value();
        }

        LOG("EmitConstantDeclaration", "Setting slot = %s", *maybe_val);
        // TODO: Support multiple declarations
        c.context().SetConstant(&node->ids()[0], *maybe_val);

        // TODO: This is a struct-speficic hack.
        if (type::Type *type_val = maybe_val->get_if<type::Type>()) {
          if (auto const *struct_type = type_val->if_as<type::Struct>()) {
            if (struct_type->completeness() != type::Completeness::Complete) {
              return *maybe_val;
            }
            // TODO: Support multiple declarations
            c.context().CompleteConstant(&node->ids()[0]);
          }
        }

        return *maybe_val;
      }
    } else if (node->IsDefaultInitialized()) {
      UNREACHABLE(node->DebugString());
    } else {
      UNREACHABLE();
    }
  }
}

ir::Value EmitNonConstantDeclaration(Compiler &c,
                                     ast::Declaration const *node) {
  if (node->IsUninitialized()) { return ir::Value(); }
  std::vector<type::Typed<ir::RegOr<ir::Addr>>> addrs;
  addrs.reserve(node->ids().size());
  for (auto const &id : node->ids()) {
    addrs.push_back(type::Typed<ir::RegOr<ir::Addr>>(
        c.context().addr(&id), c.context().qual_types(&id)[0].type()));
  }
  if (auto const *init_val = node->initial_value()) {
    c.EmitMoveInit(init_val, addrs);
  } else {
    if (not(node->flags() & ast::Declaration::f_IsFnParam)) {
      c.EmitDefaultInit(type::Typed<ir::Reg>(addrs[0]->reg(), addrs[0].type()));
    }
  }
  return ir::Value(addrs[0]->reg());
}

}  // namespace

ir::Value Compiler::EmitValue(ast::Declaration const *node) {
  LOG("Declaration", "%s", node->DebugString());
  ASSERT(node->scope()->Containing<ast::ModuleScope>()->module() ==
         &context().module());
  return (node->flags() & ast::Declaration::f_IsConst)
             ? EmitConstantDeclaration(*this, node)
             : EmitNonConstantDeclaration(*this, node);
}

ir::Value Compiler::EmitValue(ast::Declaration::Id const *node) {
  LOG("Declaration::Id", "%s", node->DebugString());
  return (node->declaration().flags() & ast::Declaration::f_IsConst)
             ? EmitConstantDeclaration(*this, &node->declaration())
             : EmitNonConstantDeclaration(*this, &node->declaration());
}


}  // namespace compiler
