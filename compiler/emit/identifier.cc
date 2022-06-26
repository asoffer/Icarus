#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "compiler/emit/compiler_common.h"
#include "compiler/emit/copy_move_assignment.h"
#include "compiler/emit/initialize.h"
#include "compiler/module.h"

namespace compiler {

void Compiler::EmitMoveInit(
    ast::Identifier const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  MoveInitializationEmitter emitter(*this);
  emitter(to[0], buffer);
}

void Compiler::EmitCopyInit(
    ast::Identifier const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  CopyInitializationEmitter emitter(*this);
  emitter(to[0], buffer);
}

void Compiler::EmitToBuffer(ast::Identifier const *node,
                            ir::PartialResultBuffer &out) {
  LOG("Identifier", "%s on context %p", node->name(), &context());
  auto decl_id_span = context().decls(node);
  ASSERT(decl_id_span.size() == 1u);
  auto const &decl_id = *decl_id_span[0];

  if (decl_id.declaration().flags() & ast::Declaration::f_IsConst) {
    EmitToBuffer(&decl_id, out);
    return;
  }
  if (decl_id.declaration().flags() & ast::Declaration::f_IsFnParam) {
    auto t                     = context().qual_types(node)[0].type();
    ir::RegOr<ir::addr_t> addr = state().addr(&decl_id);
    if ((decl_id.declaration().flags() &
         (ast::Declaration::f_IsBlockParam | ast::Declaration::f_IsOutput)) and
        not t.is_big()) {
      out.append(current_block()->Append(ir::LoadInstruction{
          .type   = t,
          .addr   = addr,
          .result = current().subroutine->Reserve(),
      }));
    } else {
      out.append(addr);
    }
  } else {
    type::Type t = context().qual_types(node)[0].type();
    auto lval    = EmitRef(node);
    if (t.is_big()) {
      out.append(lval);
    } else {
      ApplyTypes<bool, ir::Char, int8_t, int16_t, int32_t, int64_t, uint8_t,
                 uint16_t, uint32_t, uint64_t, float, double, type::Type,
                 ir::addr_t, ir::ModuleId, ir::Scope, ir::Fn>(
          t, [&]<typename T>() {
            out.append(current_block()->Append(ir::LoadInstruction{
                .type   = t,
                .addr   = lval,
                .result = current().subroutine->Reserve(),
            }));
          });
    }
  }
}

void Compiler::EmitCopyAssign(
    ast::Identifier const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  CopyAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitMoveAssign(
    ast::Identifier const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  MoveAssignmentEmitter emitter(*this);
  emitter(to[0], type::Typed(buffer[0], t));
}

ir::Reg Compiler::EmitRef(ast::Identifier const *node) {
  auto decl_id_span = context().decls(node);
  ASSERT(decl_id_span.size() == 1u);
  auto const *decl_id = decl_id_span[0];
  if (decl_id->declaration().flags() & ast::Declaration::f_IsConst) {
    auto const *buffer = context().Constant(decl_id);
    if (not buffer) {
      EmitVoid(&decl_id->declaration());
      buffer = context().Constant(decl_id);
    }
    ASSERT(buffer != nullptr);

    ir::Reg r = current().subroutine->Reserve();
    current_block()->Append(ir::RegisterInstruction<ir::addr_t>{
        .operand = const_cast<ir::addr_t>((*buffer)[0].raw().data()),
        .result  = r,
    });

    return r;
  } else {
    return state().addr(decl_id).reg();
  }
}

}  // namespace compiler
