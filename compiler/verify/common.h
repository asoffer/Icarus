#include <utility>
#include <vector>

#include "ast/ast.h"
#include "core/dependency_node.h"

namespace compiler {

// Given a parameterized expression returns a vector consisting of dependency
// nodes for each parameter declaration, and the index of that declaration.
// There are four types of dependency (representing all combinations of the type
// or value of the parameter and of the argument bound to a given parameter).
std::vector<std::pair<int, core::DependencyNode<ast::Declaration>>>
OrderedDependencyNodes(ast::ParameterizedExpression const *node);

// TODO: Document
DependentComputedData::InsertDependentResult MakeConcrete(
    ast::ParameterizedExpression const *node, CompiledModule *mod,
    absl::Span<std::pair<int, core::DependencyNode<ast::Declaration>> const>
        ordered_nodes,
    core::FnArgs<type::Typed<ir::Value>> const &args,
    DependentComputedData &compiler_data, diagnostic::DiagnosticConsumer &diag);

}  // namespace compiler
