#include "absl/types/span.h"
#include "ast/ast.h"
#include "ast/scope/fn.h"
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
                            .qual_type(&node->function_literal())
                            ->type()
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
    if (ret_type.get()->is_big()) {
      type::Typed<ir::RegOr<ir::Addr>> typed_alloc(
          ir::RegOr<ir::Addr>(builder().GetRet(i, ret_type)), ret_type);
      EmitMoveInit(expr, absl::MakeConstSpan(&typed_alloc, 1));
    } else {
      builder().SetRet(i, type::Typed<ir::Value>(EmitValue(expr), ret_type));
    }
  }

  builder().FinishTemporariesWith(
      [this](type::Typed<ir::Reg> r) { EmitDestroy(r); });

  // Rather than doing this on each block it'd be better to have each
  // scope's destructors jump you to the correct next block for destruction.
  for (auto *scope = node->scope(); scope; scope = scope->parent()) {
    MakeAllDestructions(*this, scope);
    if (scope->is<ast::FnScope>()) { break; }
  }

  builder().ReturnJump();
  return ir::Value();
}

}  // namespace compiler
