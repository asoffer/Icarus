#include <vector>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "compiler/common.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/emit/copy_move_assignment.h"
#include "compiler/emit/initialize.h"
#include "compiler/interface_instructions.h"
#include "compiler/module.h"
#include "ir/value/addr.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {
namespace {

// TODO: Support multiple declarations
void EmitConstantDeclaration(Compiler &c, ast::Declaration const *node,
                             ir::PartialResultBuffer &out) {
  Context &compilation_root = c.context().root();
  Context &node_root        = ModuleFor(node)->as<CompiledModule>().context();
  Context &ctx = (&compilation_root == &node_root) ? c.context() : node_root;

  LOG("EmitConstantDeclaration", "%s %s", node->DebugString(),
      c.context().DebugString());
  if (node->flags() & ast::Declaration::f_IsFnParam) {
    type::Type t = ctx.qual_types(&node->ids()[0])[0].type();
    if (t.is_big()) {
      ctx.LoadConstantAddress(&node->ids()[0], out);
    } else {
      ctx.LoadConstant(&node->ids()[0], out);
    }
  } else {
    if (ctx.TryLoadConstant(&node->ids()[0], out)) { return; }

    if (auto const *init_val = node->initial_value()) {
      LOG("Declaration", "Computing slot with %s",
          node->initial_value()->DebugString());

      type::Type t = ctx.qual_types(&node->ids()[0])[0].type();
      ASSIGN_OR(return,  //
                      auto value_buffer,
                      c.EvaluateToBufferOrDiagnose(
                          type::Typed<ast::Expression const *>(
                              node->initial_value(), t)));
      if (t.is_big()) {
        auto addr = c.context()
                        .SetConstant(&node->ids()[0], value_buffer)[0]
                        .raw()
                        .data();
        c.state().set_addr(&node->ids()[0], const_cast<ir::addr_t>(addr));
        out.append(addr);
      } else {
        auto addr = c.context()
                        .SetConstant(&node->ids()[0], value_buffer)[0]
                        .raw()
                        .data();
        c.state().set_addr(&node->ids()[0], const_cast<ir::addr_t>(addr));
        out.append(value_buffer);
      }
    } else if (auto const *bd = node->if_as<ast::BindingDeclaration>()) {
      if (auto const *constant = c.context().Constant(&node->ids()[0])) {
        out = *constant;
      } else {
        c.EmitToBuffer(&bd->pattern(), out);
      }
    } else if (node->IsDefaultInitialized()) {
      type::Type t = ctx.qual_types(&node->ids()[0])[0].type();
      WriteDefaultValueFor(t, out);
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
        c.state().addr(&id), c.context().qual_types(&id)[0].type()));
  }
  if (auto const *initial_value = node->initial_value()) {
    // TODO: Support multiple declarations.
    auto initial_value_qt = c.context().qual_types(initial_value)[0];
    if (initial_value_qt.type() == addrs[0].type()) {
      c.EmitMoveInit(initial_value, addrs);
    } else {
      ir::PartialResultBuffer buffer;
      c.EmitToBuffer(initial_value, buffer);
      ApplyImplicitCasts(c, initial_value_qt.type(),
                         type::QualType::NonConstant(addrs[0].type()), buffer);
      MoveInitializationEmitter emitter(c);
      emitter(addrs[0], buffer);
    }

  } else {
    if (not(node->flags() & ast::Declaration::f_IsFnParam)) {
      DefaultInitializationEmitter emitter(c);
      emitter(addrs[0].type(), *addrs[0]);
    }
  }
  out.append(addrs[0]);
}

}  // namespace

void Compiler::EmitToBuffer(ast::Declaration const *node,
                            ir::PartialResultBuffer &out) {
  LOG("Declaration", "%s on %s", node->DebugString(), context().DebugString());
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

void Compiler::EmitCopyAssign(
    ast::Declaration::Id const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  CopyAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitMoveAssign(
    ast::Declaration::Id const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  MoveAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitCopyInit(
    ast::Declaration::Id const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  CopyAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitMoveInit(
    ast::Declaration::Id const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  MoveAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitToBuffer(ast::BindingDeclaration const *node,
                            ir::PartialResultBuffer &out) {
  out.append(current_block()->Append(type::GenericTypeInstruction{
      .interface =
          ir::Interface(resources().interface_manager->UserDefined({})),
      .manager = current_block()->Append(LoadInterfaceManagerInstruction{
          .result = current().subroutine->Reserve(),
      }),
      .result  = current().subroutine->Reserve(),
  }));
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
  // TODO: If the expression evaluates to multiple types or void, we should
  // return false.
  bindings.emplace(&node->ids()[0], pmc.value);
  return true;
}

}  // namespace compiler
