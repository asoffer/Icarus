#include <utility>
#include <vector>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "compiler/context.h"
#include "compiler/module.h"
#include "compiler/verify/verify.h"
#include "core/call.h"

namespace compiler {

absl::flat_hash_set<module::Module *> ModulesFromTypeProvenance(
    absl::flat_hash_set<type::Type> const &adl_types,
    module::ModuleTable const &table);

std::optional<core::Arguments<type::Typed<ir::CompleteResultRef>>>
VerifyArguments(TypeVerifier &tv,
                absl::Span<ast::Call::Argument const> arguments,
                ir::CompleteResultBuffer &out);

std::optional<core::Parameters<type::QualType>> VerifyParameters(
    TypeVerifier &tv, core::Parameters<ast::Declaration> const &parameters);

struct VerifyCallParameters {
  ast::Expression const *call;
  CallMetadata::callee_locator_t callee;
  core::Arguments<type::Typed<ir::CompleteResultRef>> const arguments;
};

std::variant<
    type::Typed<CallMetadata::callee_locator_t>,
    absl::flat_hash_map<type::Callable const *, core::CallabilityResult>>
VerifyCall(TypeVerifier &tv, VerifyCallParameters const &vcp);

std::variant<
    std::vector<type::QualType>,
    absl::flat_hash_map<type::Callable const *, core::CallabilityResult>>
VerifyReturningCall(TypeVerifier &tv, VerifyCallParameters const &vcp);

}  // namespace compiler
