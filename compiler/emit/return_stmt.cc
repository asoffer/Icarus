#include "absl/types/span.h"
#include "ast/ast.h"
#include "ast/scope.h"
#include "compiler/compiler.h"
#include "compiler/emit/compiler_common.h"
#include "ir/value/addr.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

void Compiler::EmitToBuffer(ast::ReturnStmt const *node,
                            ir::PartialResultBuffer &out) {
  auto const &fn_type = context()
                            .qual_types(&node->function_literal())[0]
                            .type()
                            .as<type::Function>();
  // TODO: It's tricky... on a single expression that gets expanded, we could
  // have both small and big types and we would need to handle both setting
  // registers for small types and writing through them for big ones.
  ASSERT(node->exprs().size() ==
         fn_type.return_types()
             .size());  // TODO: For now, assume no actual expansion.
  for (size_t i = 0; i < node->exprs().size(); ++i) {
    auto const *expr    = node->exprs()[i];
    type::Type ret_type = fn_type.return_types()[i];
    if (ret_type.is_big()) {
      type::Typed<ir::RegOr<ir::addr_t>> typed_alloc(
          ir::RegOr<ir::addr_t>(ir::Reg::Out(i)), ret_type);
      EmitMoveInit(expr, absl::MakeConstSpan(&typed_alloc, 1));
    } else {
      out.clear();
      EmitCast(*this, context().typed(expr), ret_type, out);
      ApplyTypes<ir::Integer, bool, ir::Char, int8_t, int16_t, int32_t, int64_t,
                 uint8_t, uint16_t, uint32_t, uint64_t, float, double,
                 type::Type, ir::addr_t, ir::ModuleId, ir::Scope, ir::Fn,
                 ir::GenericFn, interface::Interface>(
          ret_type, [&]<typename T>() {
            current_block()->Append(ir::SetReturnInstruction<T>{
                .index = static_cast<uint16_t>(i),
                .value = out.back().get<T>(),
            });
          });
    }
  }

  DestroyTemporaries();
  ast::Scope const *s = node->scope();
  while (s->kind() != ast::Scope::Kind::BoundaryExecutable) { s = s->parent(); }

  current_block() = EmitDestructionPath(node->scope(), s);
  current_block()->set_jump(ir::JumpCmd::Return());
}

}  // namespace compiler
