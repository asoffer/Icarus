#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/module.h"

namespace compiler {

void Compiler::EmitMoveInit(
    ast::Identifier const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  EmitMoveInit(type::Typed<ir::Reg>(to[0]->reg(), to[0].type()), buffer);
}

void Compiler::EmitCopyInit(
    ast::Identifier const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  EmitCopyInit(type::Typed<ir::Reg>(to[0]->reg(), to[0].type()), buffer);
}

void Compiler::EmitToBuffer(ast::Identifier const *node,
                            ir::PartialResultBuffer &out) {
  LOG("Identifier", "%s on context %p", node->name(), &context());
  auto decl_id_span = context().decls(node);
  ASSERT(decl_id_span.size() == 1u);
  auto const& decl_id = *decl_id_span[0];

  if (decl_id.declaration().flags() & ast::Declaration::f_IsConst) {
    EmitToBuffer(&decl_id, out);
    return;
  }
  if (decl_id.declaration().flags() & ast::Declaration::f_IsFnParam) {
    auto t      = context().qual_types(node)[0].type();
    ir::Reg reg = builder().addr(&decl_id);
    if ((decl_id.declaration().flags() &
         (ast::Declaration::f_IsBlockParam | ast::Declaration::f_IsOutput)) and
        not t.is_big()) {
      builder().Load(reg, t, out);
    } else {
      out.append(reg);
    }
  } else {
    type::Type t = context().qual_types(node)[0].type();
    auto lval    = EmitRef(node);
    if (t.is_big()) {
      out.append(lval);
    } else {
      ApplyTypes<bool, ir::Char, ir::Integer, int8_t, int16_t, int32_t, int64_t,
                 uint8_t, uint16_t, uint32_t, uint64_t, float, double,
                 type::Type, ir::addr_t, ir::ModuleId, ir::Scope, ir::Fn,
                 ir::Jump, ir::Block, ir::GenericFn, interface::Interface>(
          t, [&]<typename T>() {
            out.append(ir::RegOr<T>(builder().PtrFix(lval, t)));
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
  EmitCopyAssign(to[0], type::Typed(buffer[0], t));
}

void Compiler::EmitMoveAssign(
    ast::Identifier const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  ir::PartialResultBuffer buffer;
  EmitToBuffer(node, buffer);
  EmitMoveAssign(to[0], type::Typed(buffer[0], t));
}

ir::Reg Compiler::EmitRef(ast::Identifier const *node) {
  auto decl_id_span = context().decls(node);
  ASSERT(decl_id_span.size() == 1u);
  return builder().addr(decl_id_span[0]);
}

}  // namespace compiler
