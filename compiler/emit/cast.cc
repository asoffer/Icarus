#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/compiler_common.h"
#include "compiler/emit/copy_move_assignment.h"
#include "compiler/emit/initialize.h"
#include "ir/value/char.h"

namespace compiler {

void Compiler::EmitCopyInit(
    ast::Cast const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  CopyInitializationEmitter emitter(*this);
  emitter(to[0], buffer);
}

void Compiler::EmitMoveInit(
    ast::Cast const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  MoveInitializationEmitter emitter(*this);
  emitter(to[0], buffer);
}

void Compiler::EmitToBuffer(ast::Cast const *node,
                            ir::PartialResultBuffer &out) {
  type::Type to_type = context().qual_types(node)[0].type();
  EmitCast(*this, context().typed(node->expr()), to_type, out);
}

void Compiler::EmitMoveAssign(
    ast::Cast const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  MoveAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitCopyAssign(
    ast::Cast const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  CopyAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

}  // namespace compiler
