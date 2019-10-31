#include "compiler/dispatch.h"

#include "base/debug.h"
#include "compiler/dispatch/extract_params.h"
#include "core/fn_params.h"
#include "type/type.h"
#include "type/typed_value.h"

namespace compiler {
namespace {

void VerifySingleOverload(Compiler *compiler,
                          type::Typed<ast::Expression const *> expr,
                          core::FnArgs<VerifyResult> const &args) {
  auto params         = ExtractParams(compiler, expr);
  auto matched_params = MatchArgsToParams(params, args);
}

}  // namespace

base::expected<DispatchTable> DispatchTable::Verify(
    Compiler *compiler, ast::OverloadSet const &os,
    core::FnArgs<VerifyResult> const &args) {
  DEBUG_LOG("verify_dispatch")
  ("Verifying overload set with ", os.members().size(), " members.");
  for (ast::Expression const *overload : os.members()) {
    // TODO the type of the specific overload could *correctly* be null and we
    // need to handle that case.
    auto typed_overload = type::Typed<ast::Expression const *>(
        overload, ASSERT_NOT_NULL(compiler->type_of(overload)));
    DEBUG_LOG("verify_dispatch")
    ("Verifying", overload, ": ", ast::Dump::ToString(overload));
    VerifySingleOverload(compiler, typed_overload, args);
  }

  return DispatchTable{};
}
}  // namespace compiler
