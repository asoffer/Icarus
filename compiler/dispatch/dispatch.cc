#include "compiler/dispatch/dispatch.h"

#include "ast/expression.h"
#include "ast/methods/dump.h"
#include "base/debug.h"
#include "compiler/dispatch/extract_params.h"
#include "core/fn_params.h"
#include "type/type.h"
#include "type/typed_value.h"
#include "type/variant.h"

namespace compiler {

base::expected<DispatchTable> DispatchTable::Verify(
    Compiler *compiler, ast::OverloadSet const &os,
    core::FnArgs<VerifyResult> const &args) {
  DEBUG_LOG("dispatch-verify")
  ("Verifying overload set with ", os.members().size(), " members.");

  // Keep a collection of failed matches around so we can give better diagnostics.
  absl::flat_hash_map<ast::Expression const *, FailedMatch> failures;
  absl::flat_hash_map<ast::Expression const *,
                      core::FnParams<type::Type const *>>
      dispatch_table;
  for (ast::Expression const *overload : os.members()) {
    // TODO the type of the specific overload could *correctly* be null and we
    // need to handle that case.
    DEBUG_LOG("dispatch-verify")
    ("Verifying ", overload, ": ", ast::Dump::ToString(overload));
    auto result = MatchArgsToParams(ExtractParams(compiler, overload), args);
    if (not result) {
      failures.emplace(overload, result.error());
    } else {
      dispatch_table.emplace(overload, *result);
    }
  }

  return DispatchTable{};
}
}  // namespace compiler
