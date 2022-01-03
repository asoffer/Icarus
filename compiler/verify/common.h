#include <utility>
#include <vector>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/context.h"
#include "compiler/module.h"

namespace compiler {

absl::flat_hash_set<module::BasicModule const *> ModulesFromTypeProvenance(
    absl::flat_hash_set<type::Type> const &adl_types);

std::vector<core::Arguments<type::QualType>> YieldArgumentTypes(
    Context const &context,
    base::PtrUnion<ast::BlockNode const, ast::ScopeNode const> node);

std::optional<core::Arguments<type::Typed<ir::CompleteResultRef>>>
VerifyArguments(Compiler &c, absl::Span<ast::Call::Argument const> arguments,
                ir::CompleteResultBuffer &out);

std::optional<core::Params<type::QualType>> VerifyParameters(
    Compiler &c, core::Params<std::unique_ptr<ast::Declaration>> const &params);

struct VerifyCallParameters {
  ast::Expression const *call;
  ast::Expression const *callee;
  core::Arguments<type::Typed<ir::CompleteResultRef>> const arguments;
};

std::variant<
    type::Typed<ast::Expression const *>,
    absl::flat_hash_map<type::Callable const *, core::CallabilityResult>>
VerifyCall(Compiler &c, VerifyCallParameters const &vcp);

std::variant<
    std::vector<type::QualType>,
    absl::flat_hash_map<type::Callable const *, core::CallabilityResult>>
VerifyReturningCall(Compiler &c, VerifyCallParameters const &vcp);

}  // namespace compiler
