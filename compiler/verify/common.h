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
    TypeVerifier &tv,
    core::Parameters<std::unique_ptr<ast::Declaration>> const &params);

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

bool ForEachSymbolQualType(ast::Scope const *start, std::string_view name,
                           std::invocable<type::QualType> auto &&fn) {
  auto const &context = start->module().as<CompiledModule>().context();
  for (ast::Scope const &s : start->ancestors()) {
    if (auto iter = s.decls_.find(name); iter != s.decls_.end()) {
      for (auto const *id : iter->second) {
        if (not fn(context.qual_types(id)[0])) { return false; }
      }
    }

    for (auto *mod : s.embedded_modules()) {
      for (auto const &symbol_info : mod->Exported(name)) {
        // TODO: what about transitivity for embedded modules?
        if (not fn(symbol_info.qualified_type)) { return false; }
      }
    }
  }
  return true;
}
}  // namespace compiler
