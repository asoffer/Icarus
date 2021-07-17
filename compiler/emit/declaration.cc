#include <vector>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "ir/value/addr.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
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
        if (not c.context().ConstantIfComplete(&node->ids()[0]) and
            c.state().must_complete) {
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

      ASSIGN_OR(return,  //
                      auto value_buffer,
                      c.EvaluateToBufferOrDiagnose(
                          type::Typed<ast::Expression const *>(
                              node->initial_value(), t)));
      LOG("EmitConstantDeclaration", "Setting slot = %s", value_buffer);

      // TODO: Support multiple declarations
      out.append(value_buffer);
      c.context().SetConstant(&node->ids()[0], value_buffer);

      // TODO: This is a struct-speficic hack.
      if (t == type::Type_) {
        if (auto const *struct_type =
                value_buffer.get<type::Type>(0).if_as<type::Struct>()) {
          if (struct_type->completeness() != type::Completeness::Complete) {
            out.append(value_buffer.get<type::Type>(0));
            return;
          }
          // TODO: Support multiple declarations
          c.context().CompleteConstant(&node->ids()[0]);
        }
      }

      return;
    } else if (auto const *bd = node->if_as<ast::BindingDeclaration>()) {
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
    absl::flat_hash_map<ast::Declaration::Id const *, ir::CompleteResultBuffer>
        &bindings) {
  NOT_YET();
}

bool Compiler::PatternMatch(
    ast::BindingDeclaration const *node, PatternMatchingContext &pmc,
    absl::flat_hash_map<ast::Declaration::Id const *, ir::CompleteResultBuffer>
        &bindings) {
  if (auto const *p = pmc.type.if_as<type::Primitive>()) {
    bindings.emplace(&node->ids()[0], pmc.value);
    return true;
  } else {
    NOT_YET(pmc.type.to_string());
  }
}

}  // namespace compiler
