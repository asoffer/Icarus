#include "ast/ast.h"
#include "compiler/compiler.h"

namespace compiler {

void Compiler::EmitMoveInit(
    ast::Identifier const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  base::untyped_buffer buffer;
  EmitToBuffer(node, buffer);
  EmitMoveInit(type::Typed<ir::Reg>(to[0]->reg(), to[0].type()), buffer);
}

void Compiler::EmitCopyInit(
    ast::Identifier const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  base::untyped_buffer buffer;
  EmitToBuffer(node, buffer);
  EmitCopyInit(type::Typed<ir::Reg>(to[0]->reg(), to[0].type()), buffer);
}

void Compiler::EmitToBuffer(ast::Identifier const *node,
                            base::untyped_buffer &out) {
  LOG("Identifier", "%s on context %p", node->name(), &context());
  auto decl_span = context().decls(node);
  ASSERT(decl_span.size() != 0u);
  if (decl_span[0]->flags() & ast::Declaration::f_IsConst) {
    auto const *mod = &decl_span[0]
                           ->scope()
                           ->Containing<ast::ModuleScope>()
                           ->module()
                           ->as<CompiledModule>();
    if (mod != &context().module()) {
      auto value = mod->context(&context().module())
                       .Constant(&decl_span[0]->ids()[0])
                       ->value();
      ApplyTypes<bool, ir::Char, ir::Integer, int8_t, int16_t, int32_t, int64_t,
                 uint8_t, uint16_t, uint32_t, uint64_t, float, double,
                 type::Type, ir::addr_t, ir::ModuleId, ir::Scope, ir::Fn,
                 ir::Jump, ir::Block, ir::GenericFn, interface::Interface>(
          context().qual_types(node)[0].type(),
          [&]<typename T>() { out.append(value.get<ir::RegOr<T>>()); });
    } else {
      EmitToBuffer(decl_span[0], out);
    }
    return;
  }
  if (decl_span[0]->flags() & ast::Declaration::f_IsFnParam) {
    auto t = context().qual_types(node)[0].type();
    // TODO: Support multiple declarations
    ir::Reg reg = builder().addr(&decl_span[0]->ids()[0]);
    if ((decl_span[0]->flags() &
         (ast::Declaration::f_IsBlockParam | ast::Declaration::f_IsOutput)) and
        not t.get()->is_big()) {
      ApplyTypes<bool, ir::Char, ir::Integer, int8_t, int16_t, int32_t, int64_t,
                 uint8_t, uint16_t, uint32_t, uint64_t, float, double,
                 type::Type, ir::addr_t, ir::ModuleId, ir::Scope, ir::Fn,
                 ir::Jump, ir::Block, ir::GenericFn, interface::Interface>(
          t, [&]<typename T>() {
            out.append(builder().Load(reg, t).get<ir::RegOr<T>>());
          });
    } else {
      ApplyTypes<bool, ir::Char, ir::Integer, int8_t, int16_t, int32_t, int64_t,
                 uint8_t, uint16_t, uint32_t, uint64_t, float, double,
                 type::Type, ir::addr_t, ir::ModuleId, ir::Scope, ir::Fn,
                 ir::Jump, ir::Block, ir::GenericFn, interface::Interface>(
          t, [&]<typename T>() {
            out.append(ir::RegOr<T>(builder().PtrFix(reg, t)));
          });
    }
  } else {
    type::Type t = context().qual_types(node)[0].type();
    auto lval    = EmitRef(node);
    ApplyTypes<bool, ir::Char, ir::Integer, int8_t, int16_t, int32_t, int64_t,
               uint8_t, uint16_t, uint32_t, uint64_t, float, double, type::Type,
               ir::addr_t, ir::ModuleId, ir::Scope, ir::Fn, ir::Jump, ir::Block,
               ir::GenericFn, interface::Interface>(t, [&]<typename T>() {
      out.append(ir::RegOr<T>(builder().PtrFix(lval, t)));
    });
  }
}

void Compiler::EmitCopyAssign(
    ast::Identifier const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  base::untyped_buffer buffer;
  EmitToBuffer(node, buffer);
  EmitCopyAssign(to[0], ValueView(t, buffer));
}

void Compiler::EmitMoveAssign(
    ast::Identifier const *node,
    absl::Span<type::Typed<ir::RegOr<ir::addr_t>> const> to) {
  ASSERT(to.size() == 1u);
  auto t = context().qual_types(node)[0].type();
  base::untyped_buffer buffer;
  EmitToBuffer(node, buffer);
  EmitMoveAssign(to[0], ValueView(t, buffer));
}

ir::Reg Compiler::EmitRef(ast::Identifier const *node) {
  auto decl_span = context().decls(node);
  ASSERT(decl_span.size() == 1u);
  for (auto const &id : decl_span[0]->ids()) {
    if (id.name() != node->name()) { continue; }
    return builder().addr(&id);
  }
  UNREACHABLE();
}

}  // namespace compiler
