#include <utility>
#include <vector>

#include "absl/types/span.h"
#include "ast/ast.h"
#include "compiler/compiler.h"
#include "compiler/context.h"
#include "compiler/module.h"

namespace compiler {

std::vector<core::Arguments<type::QualType>> YieldArgumentTypes(
    Context const &context,
    base::PtrUnion<ast::BlockNode const, ast::ScopeNode const> node);

std::optional<core::Arguments<type::Typed<ir::CompleteResultRef>>>
VerifyArguments(Compiler &c, absl::Span<ast::Call::Argument const> arguments,
                ir::CompleteResultBuffer &out);

type::QualType VerifyCallee(Compiler &c, ast::Expression const *callee,
                            absl::flat_hash_set<type::Type> const &adl_types);

struct VerifyCallParameters{
  ast::Expression const *callee;
  core::Arguments<type::Typed<ir::CompleteResultRef>> const arguments;
};

std::variant<ast::OverloadSet, absl::flat_hash_map<type::Callable const *,
                                                   core::CallabilityResult>>
VerifyCall(Compiler &c, VerifyCallParameters const &vcp);

std::variant<
    std::vector<type::QualType>,
    absl::flat_hash_map<type::Callable const *, core::CallabilityResult>>
VerifyReturningCall(Compiler &c, VerifyCallParameters const &vcp);

}  // namespace compiler
