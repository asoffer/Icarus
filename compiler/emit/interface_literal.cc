#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/emit/copy_move_assignment.h"
#include "compiler/module.h"
#include "ir/value/result_buffer.h"
#include "type/typed_value.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::InterfaceLiteral const *node,
                            ir::PartialResultBuffer &out) {
  // TODO: Finish implementation.
  out.append(resources().interface_manager->UserDefined());
}

void Compiler::EmitCopyAssign(
    ast::InterfaceLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  CopyAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitMoveAssign(
    ast::InterfaceLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  MoveAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitCopyInit(
    ast::InterfaceLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  CopyAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitMoveInit(
    ast::InterfaceLiteral const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  auto t = context().qual_types(node)[0].type();
  ASSERT(to.size() == 1u);
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  MoveAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

}  // namespace compiler

