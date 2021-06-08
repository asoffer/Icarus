#include "absl/cleanup/cleanup.h"
#include "ast/ast.h"
#include "ast/build_param_dependency_graph.h"
#include "ast/scope.h"

namespace ast {
namespace {

// Given a parameterized expression returns a vector consisting of dependency
// nodes for each parameter declaration, and the index of that declaration.
// There are four types of dependency (representing all combinations of the type
// or value of the parameter and of the argument bound to a given parameter).
//
// TODO: OrderedDependencyNodes does not need any information not already
// available on the AST. We should compute this for all parameterized
// expressions, or at least those with generics and stash it.
std::vector<std::pair<int, core::DependencyNode<ast::Declaration>>>
OrderedDependencyNodes(ast::ParameterizedExpression const* node) {
  absl::flat_hash_set<core::DependencyNode<ast::Declaration>> deps;
  for (auto const& p : node->params()) {
    deps.insert(
        core::DependencyNode<ast::Declaration>::MakeArgType(p.value.get()));
    deps.insert(
        core::DependencyNode<ast::Declaration>::MakeType(p.value.get()));
    if (p.value->flags() & ast::Declaration::f_IsConst) {
      deps.insert(
          core::DependencyNode<ast::Declaration>::MakeValue(p.value.get()));
      deps.insert(
          core::DependencyNode<ast::Declaration>::MakeArgValue(p.value.get()));
    }
  }

  std::vector<std::pair<int, core::DependencyNode<ast::Declaration>>>
      ordered_nodes;
  ordered_nodes.reserve(4 * deps.size());
  BuildParamDependencyGraph(node->params()).topologically([&](auto dep_node) {
    if (not deps.contains(dep_node)) { return; }
    LOG("OrderedDependencyNodes", "adding %s`%s`", ToString(dep_node.kind()),
        absl::StrJoin(dep_node.node()->ids(), ", ",
                      [](std::string* out, ast::Declaration::Id const& id) {
                        absl::StrAppend(out, id.name());
                      }));
    ordered_nodes.emplace_back(0, dep_node);
  });

  // Compute and set the index or `ordered_nodes` so that each node knows the
  // ordering in source code. This allows us to match parameters to arguments
  // efficiently.
  absl::flat_hash_map<ast::Declaration const*, int> param_index;
  int index = 0;
  for (auto const& param : node->params()) {
    param_index.emplace(param.value.get(), index++);
  }

  for (auto& [index, node] : ordered_nodes) {
    index = param_index.find(node.node())->second;
  }

  return ordered_nodes;
}

}  // namespace

template <typename T>
static void InitializeAll(std::vector<std::unique_ptr<T>>& nodes,
                          Node::Initializer& initializer) {
  for (auto& n : nodes) { n->Initialize(initializer); }
}

void InitializeNodes(base::PtrSpan<Node> nodes,
                     Node::Initializer& initializer) {
  for (auto* n : nodes) { n->Initialize(initializer); }
}

void Access::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  operand_->Initialize(initializer);
}

void ArgumentType::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
}

void ArrayLiteral::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  for (auto& expr : elems_) { expr->Initialize(initializer); }
}

void ArrayType::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  for (auto const& len : lengths_) { len->Initialize(initializer); }
  data_type_->Initialize(initializer);
}

void Assignment::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  InitializeAll(lhs_, initializer);
  InitializeAll(rhs_, initializer);
}

void BinaryOperator::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  lhs_->Initialize(initializer);
  rhs_->Initialize(initializer);
}

void BlockLiteral::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  set_body_with_parent(initializer.scope);
  initializer.scope = &body_scope();
  absl::Cleanup c = [&] { initializer.scope = scope_; };
  InitializeAll(before_, initializer);
  InitializeAll(after_, initializer);
}

void BlockNode::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  set_body_with_parent(initializer.scope, true);
  initializer.scope = &body_scope();
  absl::Cleanup c = [&] { initializer.scope = scope_; };
  for (auto& param : params_) { param.value->Initialize(initializer); }
  InitializeAll(stmts_, initializer);
}

void BuiltinFn::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
}

void Call::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  callee_->Initialize(initializer);
  for (auto& arg : arguments_) { arg.expr().Initialize(initializer); }
}

void Cast::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  expr_->Initialize(initializer);
  type_->Initialize(initializer);
}

void ComparisonOperator::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  for (auto& expr : exprs_) { expr->Initialize(initializer); }
}

void Declaration::Initialize(Initializer& initializer) {
  ASSERT(initializer.scope != nullptr);
  scope_ = initializer.scope;
  scope_->InsertDeclaration(this);
  if (type_expr_.get()) { type_expr_->Initialize(initializer); }
  if (init_val_.get()) { init_val_->Initialize(initializer); }
  for (auto& id : ids_) { id.Initialize(initializer); }
}

void DesignatedInitializer::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  type_->Initialize(initializer);
  for (auto& assignment : assignments_) { assignment->Initialize(initializer); }
}

void EnumLiteral::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  set_body_with_parent(initializer.scope);
  for (auto& [id, value] : values_) { value->Initialize(initializer); }
}

void FunctionLiteral::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  set_body_with_parent(initializer.scope);

  initializer.scope = &body_scope();
  auto const* f     = std::exchange(initializer.function_literal, this);
  absl::Cleanup c   = [&] {
    initializer.scope            = scope_;
    initializer.function_literal = f;
  };
  for (auto& param : params_) { param.value->Initialize(initializer); }
  if (outputs_) {
    for (auto& out : *outputs_) { out->Initialize(initializer); }
  }
  InitializeAll(stmts_, initializer);
  ordered_dependency_nodes_ = OrderedDependencyNodes(this);
}

void FunctionType::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  InitializeAll(params_, initializer);
  InitializeAll(output_, initializer);
}

void Identifier::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
}

void Import::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  operand_->Initialize(initializer);
}

void Index::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  lhs_->Initialize(initializer);
  rhs_->Initialize(initializer);
}

void InterfaceLiteral::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  set_body_with_parent(initializer.scope);
  initializer.scope = &body_scope();
  absl::Cleanup c = [&] { initializer.scope = scope_; };
  for (auto& [name, expr] : entries_) {
    name->Initialize(initializer);
    expr->Initialize(initializer);
  }
}

void ConditionalGoto::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  condition_->Initialize(initializer);
  for (auto& opt : true_options_) {
    opt.args_.Apply([&](auto& expr) { expr->Initialize(initializer); });
  }
  for (auto& opt : false_options_) {
    opt.args_.Apply([&](auto& expr) { expr->Initialize(initializer); });
  }
}

void UnconditionalGoto::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  for (auto& opt : options_) {
    opt.args_.Apply([&](auto& expr) { expr->Initialize(initializer); });
  }
}

void Jump::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  set_body_with_parent(initializer.scope);
  initializer.scope = &body_scope();
  absl::Cleanup c = [&] { initializer.scope = scope_; };
  if (state_.get()) { state_->Initialize(initializer); }
  for (auto& param : params_) { param.value->Initialize(initializer); }
  InitializeAll(stmts_, initializer);
}

void Label::Initialize(Initializer& initializer) { scope_ = initializer.scope; }

void ReturnStmt::Initialize(Initializer& initializer) {
  scope_            = initializer.scope;
  function_literal_ = initializer.function_literal;
  InitializeAll(exprs_, initializer);
}

void YieldStmt::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  InitializeAll(exprs_, initializer);
}

void ScopeLiteral::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  set_body_with_parent(initializer.scope);
  if (state_type_) { state_type_->Initialize(initializer); }
  initializer.scope = &body_scope();
  absl::Cleanup c = [&] { initializer.scope = scope_; };
  for (auto& decl : decls_) { decl.Initialize(initializer); }
}

void ScopeNode::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  name_->Initialize(initializer);
  args_.Apply([&](Expression* expr) { expr->Initialize(initializer); });

  for (auto& block : blocks_) { block.Initialize(initializer); }
}

void SliceType::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  data_type_->Initialize(initializer);
}

void ShortFunctionLiteral::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  set_body_with_parent(initializer.scope);
  initializer.scope = &body_scope();
  absl::Cleanup c = [&] { initializer.scope = scope_; };
  for (auto& param : params_) { param.value->Initialize(initializer); }
  body_->Initialize(initializer);
  ordered_dependency_nodes_ = OrderedDependencyNodes(this);
}

void StructLiteral::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  set_body_with_parent(initializer.scope);
  initializer.scope = &body_scope();
  absl::Cleanup c = [&] { initializer.scope = scope_; };
  for (auto& field : fields_) { field.Initialize(initializer); }
}

void ParameterizedStructLiteral::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  set_body_with_parent(initializer.scope);
  initializer.scope = &body_scope();
  absl::Cleanup c = [&] { initializer.scope = scope_; };
  for (auto& param : params_) { param.value->Initialize(initializer); }
  for (auto& field : fields_) { field.Initialize(initializer); }
  ordered_dependency_nodes_ = OrderedDependencyNodes(this);
}

void Terminal::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
}

void UnaryOperator::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  operand_->Initialize(initializer);
}

}  // namespace ast
