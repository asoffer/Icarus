#include "absl/types/span.h"
#include "ast/ast.h"
#include "ast/scope.h"
#include "compiler/compiler.h"
#include "compiler/emit/common.h"
#include "ir/value/addr.h"
#include "ir/value/reg.h"
#include "ir/value/reg_or.h"
#include "ir/value/value.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {

ir::Value Compiler::EmitValue(ast::ReturnStmt const *node) {
  auto const &fn_type = context()
                            .qual_types(&node->function_literal())[0]
                            .type()
                            .as<type::Function>();

  // TODO: Reduce code-size by sharing these sequences whenever they share a
  // suffix.
  for (auto iter = state().scope_landings.rbegin();
       iter != state().scope_landings.rend(); ++iter) {
    // TODO: Emit all destructions on this scope.
    // TODO: Call the quick-exit for this scope.
  }

  // TODO: It's tricky... on a single expression that gets expanded, we could
  // have both small and big types and we would need to handle both setting
  // registers for small types and writing through them for big ones.
  ASSERT(
      node->exprs().size() ==
      fn_type.output().size());  // TODO: For now, assume no actual expansion.
  for (size_t i = 0; i < node->exprs().size(); ++i) {
    auto const *expr    = node->exprs()[i];
    type::Type ret_type = fn_type.output()[i];
    if (ret_type.is_big()) {
      type::Typed<ir::RegOr<ir::addr_t>> typed_alloc(
          ir::RegOr<ir::addr_t>(ir::Reg::Out(i)), ret_type);
      EmitMoveInit(expr, absl::MakeConstSpan(&typed_alloc, 1));
    } else {
      ApplyTypes<bool, ir::Char, int8_t, int16_t, int32_t, int64_t, uint8_t,
                 uint16_t, uint32_t, uint64_t, float, double, type::Type,
                 ir::addr_t, ir::ModuleId, ir::Scope, ir::Fn, ir::Jump, ir::Block,
                 ir::GenericFn,
                 interface::Interface>(ret_type, [&]<typename T>() {
        auto value = EmitValue(expr).get<ir::RegOr<T>>();
        builder().CurrentBlock()->Append(ir::SetReturnInstruction<T>{
            .index = static_cast<uint16_t>(i),
            .value = value,
        });
      });
    }
  }

  builder().FinishTemporariesWith(
      [this](type::Typed<ir::Reg> r) { EmitDestroy(r); });

  // Rather than doing this on each block it'd be better to have each
  // scope's destructors jump you to the correct next block for destruction.
  for (auto const &scope : node->scope()->ancestors()) {
    MakeAllDestructions(*this, &scope);
    if (scope.is<ast::FnScope>()) { break; }
  }

  builder().ReturnJump();
  return ir::Value();
}

}  // namespace compiler
