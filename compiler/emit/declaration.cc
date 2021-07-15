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

void EmitConstantDeclaration(Compiler &c, ast::Declaration const *node,
                             ir::PartialResultBuffer &out) {
  // TODO: Support multiple declarations
  auto qts = c.context().qual_types(&node->ids()[0]);
  auto t   = qts[0].type();

  LOG("EmitConstantDeclaration", "%s", node->DebugString());
  if (node->flags() & ast::Declaration::f_IsFnParam) {
    c.context().LoadConstant(&node->ids()[0], out);
  } else {
    // TODO: Support multiple declarations.
    if (auto *constant_value = c.context().Constant(&node->ids()[0])) {
      // TODO: This feels quite hacky.
      if (node->init_val()->is<ast::StructLiteral>()) {
        // TODO:
        if (not true /*constant_value->complete*/ and c.state().must_complete) {
          LOG("compile-work-queue", "Request work complete-struct: %p", node);
          c.Enqueue({
              .kind      = WorkItem::Kind::CompleteStructMembers,
              .node      = node->init_val(),
              .resources = c.resources(),
          });
        }
      }
      out = *constant_value;
      return;
    }

    if (auto const *init_val = node->initial_value()) {
      LOG("Declaration", "Computing slot with %s",
          node->initial_value()->DebugString());

      if (t.is_big()) {
        auto value_buffer = c.EvaluateToBufferOrDiagnose(
            type::Typed<ast::Expression const *>(node->initial_value(), t));
        if (auto *diagnostics =
                std::get_if<std::vector<diagnostic::ConsumedMessage>>(
                    &value_buffer)) {
          for (auto &d : *diagnostics) { c.diag().Consume(std::move(d)); }
          return;
        }

        LOG("EmitConstantDeclaration", "Setting slot = %s", value_buffer);

        out.append(std::get<ir::CompleteResultBuffer>(value_buffer));
        c.context().SetConstant(
            &node->ids()[0], std::get<ir::CompleteResultBuffer>(value_buffer));
        return;
      } else {
        auto maybe_result = c.EvaluateToBufferOrDiagnose(
            type::Typed<ast::Expression const *>(node->initial_value(), t));
        if (auto const *diagnostics =
                std::get_if<std::vector<diagnostic::ConsumedMessage>>(
                    &maybe_result)) {
          for (auto &d : *diagnostics) { c.diag().Consume(std::move(d)); }
          return;
        }

        auto &result = std::get<ir::CompleteResultBuffer>(maybe_result);
        LOG("EmitConstantDeclaration", "Setting slot = %s", result);
        // TODO: Support multiple declarations
        c.context().SetConstant(&node->ids()[0], result);

        // TODO: This is a struct-speficic hack.
        if (t == type::Type_) {
          if (auto const *struct_type =
                  result.get<type::Type>(0).if_as<type::Struct>()) {
            if (struct_type->completeness() != type::Completeness::Complete) {
              out.append(result.get<type::Type>(0));
              return;
            }
            // TODO: Support multiple declarations
            // c.context().CompleteConstant(&node->ids()[0]);
          }
        }

        out = result;
        return;
      }
    } else if (auto const * bd =node->if_as<ast::BindingDeclaration>()) {
      // TODO: Support multiple declarations
      if (auto const *constant = c.context().Constant(&node->ids()[0])) {
        out = *constant;
      } else {
        c.EmitToBuffer(&bd->pattern(), out);
      }
      return;
    } else if (node->IsDefaultInitialized()) {
      UNREACHABLE(node->DebugString());
    } else {
      UNREACHABLE();
    }
  }
}

void EmitNonConstantDeclaration(Compiler &c, ast::Declaration const *node,
                                ir::PartialResultBuffer &out) {
  if (node->IsUninitialized()) { return; }
  std::vector<type::Typed<ir::RegOr<ir::addr_t>>> addrs;
  addrs.reserve(node->ids().size());
  for (auto const &id : node->ids()) {
    addrs.push_back(type::Typed<ir::RegOr<ir::addr_t>>(
        c.builder().addr(&id), c.context().qual_types(&id)[0].type()));
  }
  if (auto const *init_val = node->initial_value()) {
    c.EmitMoveInit(init_val, addrs);
  } else {
    if (not(node->flags() & ast::Declaration::f_IsFnParam)) {
      c.EmitDefaultInit(type::Typed<ir::Reg>(addrs[0]->reg(), addrs[0].type()));
    }
  }
  out.append(addrs[0]);
}

}  // namespace

void Compiler::EmitToBuffer(ast::Declaration const *node,
                            ir::PartialResultBuffer &out) {
  LOG("Declaration", "%s", node->DebugString());
  ASSERT(node->scope()->Containing<ast::ModuleScope>()->module() ==
         &context().module());
  if (node->flags() & ast::Declaration::f_IsConst) {
    EmitConstantDeclaration(*this, node, out);
  } else {
    EmitNonConstantDeclaration(*this, node, out);
  }
}

void Compiler::EmitToBuffer(ast::Declaration::Id const *node,
                            ir::PartialResultBuffer &out) {
  LOG("Declaration::Id", "%s", node->DebugString());
  return (node->declaration().flags() & ast::Declaration::f_IsConst)
             ? EmitConstantDeclaration(*this, &node->declaration(), out)
             : EmitNonConstantDeclaration(*this, &node->declaration(), out);
}

void Compiler::EmitToBuffer(ast::BindingDeclaration const *node,
                            ir::PartialResultBuffer &) {
  UNREACHABLE();
}

bool Compiler::PatternMatch(
    ast::Declaration const *node, PatternMatchingContext &pmc,
    absl::flat_hash_map<ast::Declaration::Id const *, ir::Value> &bindings) {
  NOT_YET();
}

bool Compiler::PatternMatch(
    ast::BindingDeclaration const *node, PatternMatchingContext &pmc,
    absl::flat_hash_map<ast::Declaration::Id const *, ir::Value> &bindings) {
  if (auto const *p = pmc.type.if_as<type::Primitive>()) {
    bindings.emplace(&node->ids()[0], p->Apply([&]<typename T>()->ir::Value {
      return ir::Value(pmc.value.template get<T>(0));
    }));
    return true;
  } else {
    NOT_YET(pmc.type.to_string());
  }
}

}  // namespace compiler
