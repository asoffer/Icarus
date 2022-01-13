#include <utility>
#include <vector>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "compiler/context.h"
#include "compiler/module.h"
#include "compiler/verify/verify.h"
#include "core/call.h"

namespace compiler {

absl::flat_hash_set<module::BasicModule const *> ModulesFromTypeProvenance(
    absl::flat_hash_set<type::Type> const &adl_types);

std::optional<core::Arguments<type::Typed<ir::CompleteResultRef>>>
VerifyArguments(TypeVerifier &tv,
                absl::Span<ast::Call::Argument const> arguments,
                ir::CompleteResultBuffer &out);

std::optional<core::Params<type::QualType>> VerifyParameters(
    TypeVerifier &tv,
    core::Params<std::unique_ptr<ast::Declaration>> const &params);

struct VerifyCallParameters {
  ast::Expression const *call;
  ast::Expression const *callee;
  core::Arguments<type::Typed<ir::CompleteResultRef>> const arguments;
};

std::variant<
    type::Typed<ast::Expression const *>,
    absl::flat_hash_map<type::Callable const *, core::CallabilityResult>>
VerifyCall(TypeVerifier &tv, VerifyCallParameters const &vcp);

std::variant<
    std::vector<type::QualType>,
    absl::flat_hash_map<type::Callable const *, core::CallabilityResult>>
VerifyReturningCall(TypeVerifier &tv, VerifyCallParameters const &vcp);

// Calls `fn` on each declaration in this scope and in parent scopes with the
// given identifier `name`, until calling `fn` returns false.
bool ForEachDeclIdTowardsRoot(
    ast::Scope const *start, std::string_view name,
    std::invocable<ast::Declaration::Id const *> auto &&fn) {
  for (ast::Scope const &s : start->ancestors()) {
    if (auto iter = s.decls_.find(name); iter != s.decls_.end()) {
      for (auto const *id : iter->second) {
        if (not fn(id)) { return false; }
      }
    }

    for (auto const *mod : s.embedded_modules()) {
      for (auto const *id : mod->ExportedDeclarationIds(name)) {
        // TODO what about transitivity for embedded modules?
        // New context will lookup with no constants.
        if (not fn(id)) { return false; }
      }
    }
  }
  return true;
}

}  // namespace compiler
