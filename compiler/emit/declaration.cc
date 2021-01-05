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
  if (node->module() != &c.context().module()) {
    // Constant declarations from other modules should already be stored on
    // that module. They must be at the root of the binding tree map,
    // otherwise they would be local to some function/jump/etc. and not be
    // exported.
    return node->module()
        ->as<CompiledModule>()
        .context()
        .Constant(node)
        ->value();
  }

  if (node->flags() & ast::Declaration::f_IsFnParam) {
    auto val = c.context().LoadConstantParam(node);
    LOG("Declaration", "%s", val);
    return val;
  } else {
    if (auto *constant_value = c.context().Constant(node)) {
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

    auto t = ASSERT_NOT_NULL(c.context().qual_type(node))->type();

    if (node->IsCustomInitialized()) {
      LOG("Declaration", "Computing slot with %s",
          node->init_val()->DebugString());

      if (t.get()->is_big()) {
        auto value_buffer = c.EvaluateToBufferOrDiagnose(
            type::Typed<ast::Expression const *>(node->init_val(), t));
        if (value_buffer.empty()) { return ir::Value(); }

        LOG("EmitValueDeclaration", "Setting slot = %s", value_buffer);
        return c.context().SetConstant(node, std::move(value_buffer));
      } else {
        auto maybe_val = c.Evaluate(
            type::Typed<ast::Expression const *>(node->init_val(), t),
            c.state().must_complete);
        if (not maybe_val) {
          // TODO: we reserved a slot and haven't cleaned it up. Do we care?
          c.diag().Consume(maybe_val.error());
          return ir::Value();
        }

        LOG("EmitValueDeclaration", "Setting slot = %s", *maybe_val);
        c.context().SetConstant(node, *maybe_val);

        // TODO: This is a struct-speficic hack.
        if (type::Type *type_val = maybe_val->get_if<type::Type>()) {
          if (auto const *struct_type = type_val->if_as<type::Struct>()) {
            if (struct_type->completeness() != type::Completeness::Complete) {
              return *maybe_val;
            }
            c.context().CompleteConstant(node);
          }
        }

        return *maybe_val;
      }
    } else if (node->IsDefaultInitialized()) {
      UNREACHABLE();
    } else {
      UNREACHABLE();
    }
  }
}

ir::Value EmitNonConstantDeclaration(Compiler &c,
                                     ast::Declaration const *node) {
  if (node->IsUninitialized()) { return ir::Value(); }
  auto t = c.context().qual_type(node)->type();
  auto a = c.context().addr(node);
  if (node->IsCustomInitialized()) {
    auto to = type::Typed<ir::RegOr<ir::Addr>>(a, t);
    c.EmitMoveInit(node->init_val(), absl::MakeConstSpan(&to, 1));
  } else {
    if (not(node->flags() & ast::Declaration::f_IsFnParam)) {
      c.EmitDefaultInit(type::Typed<ir::Reg>(a, t));
    }
  }
  return ir::Value(a);
}

}  // namespace

ir::Value Compiler::EmitValue(ast::Declaration const *node) {
  LOG("Declaration", "%s", node->id());
  return (node->flags() & ast::Declaration::f_IsConst)
             ? EmitConstantDeclaration(*this, node)
             : EmitNonConstantDeclaration(*this, node);
}

}  // namespace compiler
