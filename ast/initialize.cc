#include "ast/ast.h"
#include "ast/build_param_dependency_graph.h"
#include "ast/scope/decl.h"

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
OrderedDependencyNodes(ast::ParameterizedExpression const *node) {
  absl::flat_hash_set<core::DependencyNode<ast::Declaration>> deps;
  for (auto const &p : node->params()) {
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
                      [](std::string *out, ast::Declaration::Id const &id) {
                        absl::StrAppend(out, id.name());
                      }));
    ordered_nodes.emplace_back(0, dep_node);
  });

  // Compute and set the index or `ordered_nodes` so that each node knows the
  // ordering in source code. This allows us to match parameters to arguments
  // efficiently.
  absl::flat_hash_map<ast::Declaration const *, int> param_index;
  int index = 0;
  for (auto const &param : node->params()) {
    param_index.emplace(param.value.get(), index++);
  }

  for (auto &[index, node] : ordered_nodes) {
    index = param_index.find(node.node())->second;
  }

  return ordered_nodes;
}

}  // namespace

template <typename T>
static void SetAllScopes(std::vector<std::unique_ptr<T>> *nodes, Scope *scope) {
  for (auto &n : *nodes) { n->Initialize(scope); }
}

void InitializeNodes(base::PtrSpan<Node> nodes, Scope *scope) {
  for (auto *n : nodes) { n->Initialize(scope); }
}

void Access::Initialize(Scope *scope) {
  scope_ = scope;
  operand_->Initialize(scope);
}

void ArgumentType::Initialize(Scope *scope) { scope_ = scope; }

void ArrayLiteral::Initialize(Scope *scope) {
  scope_ = scope;
  for (auto &expr : elems_) { expr->Initialize(scope); }
}

void ArrayType::Initialize(Scope *scope) {
  scope_ = scope;
  for (auto const &len : lengths_) { len->Initialize(scope); }
  data_type_->Initialize(scope);
}

void Assignment::Initialize(Scope *scope) {
  scope_ = scope;
  SetAllScopes(&lhs_, scope);
  SetAllScopes(&rhs_, scope);
}

void BinaryOperator::Initialize(Scope *scope) {
  scope_ = scope;
  lhs_->Initialize(scope);
  rhs_->Initialize(scope);
}

void BlockLiteral::Initialize(Scope *scope) {
  scope_ = scope;
  set_body_with_parent(scope);
  SetAllScopes(&before_, body_scope());
  SetAllScopes(&after_, body_scope());
}

void BlockNode::Initialize(Scope *scope) {
  scope_ = scope;
  set_body_with_parent(scope);
  for (auto &param : params_) { param.value->Initialize(body_scope()); }
  SetAllScopes(&stmts_, body_scope());
}

void BuiltinFn::Initialize(Scope *scope) { scope_ = scope; }

void Call::Initialize(Scope *scope) {
  scope_ = scope;
  callee_->Initialize(scope);
  for (auto &[name, expr] : arguments_) { expr->Initialize(scope); }
}

void Cast::Initialize(Scope *scope) {
  scope_ = scope;
  expr_->Initialize(scope);
  type_->Initialize(scope);
}

void ComparisonOperator::Initialize(Scope *scope) {
  scope_ = scope;
  for (auto &expr : exprs_) { expr->Initialize(scope); }
}

void Declaration::Initialize(Scope *scope) {
  ASSERT(scope != nullptr);
  scope_ = scope;
  scope_->InsertDeclaration(this);
  if (type_expr_.get()) { type_expr_->Initialize(scope); }
  if (init_val_.get()) { init_val_->Initialize(scope); }
  for (auto &id : ids_) { id.Initialize(scope); }
}

void DesignatedInitializer::Initialize(Scope *scope) {
  scope_ = scope;
  type_->Initialize(scope);
  for (auto &assignment : assignments_) { assignment->Initialize(scope); }
}

void EnumLiteral::Initialize(Scope *scope) {
  scope_ = scope;
  set_body_with_parent(scope);
  for (auto &[id, value] : values_) { value->Initialize(scope); }
}

void FunctionLiteral::Initialize(Scope *scope) {
  scope_ = scope;
  set_body_with_parent(scope, this);

  for (auto &param : params_) { param.value->Initialize(body_scope()); }
  if (outputs_) {
    for (auto &out : *outputs_) { out->Initialize(body_scope()); }
  }
  SetAllScopes(&stmts_, body_scope());
  ordered_dependency_nodes_ = OrderedDependencyNodes(this);
}

void FunctionType::Initialize(Scope *scope) {
  scope_ = scope;
  SetAllScopes(&params_, scope);
  SetAllScopes(&output_, scope);
}

void Identifier::Initialize(Scope *scope) { scope_ = scope; }

void Import::Initialize(Scope *scope) {
  scope_ = scope;
  operand_->Initialize(scope);
}

void Index::Initialize(Scope *scope) {
  scope_ = scope;
  lhs_->Initialize(scope);
  rhs_->Initialize(scope);
}

void ConditionalGoto::Initialize(Scope *scope) {
  scope_ = scope;
  condition_->Initialize(scope);
  for (auto &opt : true_options_) {
    opt.args_.Apply([scope](auto &expr) { expr->Initialize(scope); });
  }
  for (auto &opt : false_options_) {
    opt.args_.Apply([scope](auto &expr) { expr->Initialize(scope); });
  }
}

void UnconditionalGoto::Initialize(Scope *scope) {
  scope_ = scope;
  for (auto &opt : options_) {
    opt.args_.Apply([scope](auto &expr) { expr->Initialize(scope); });
  }
}

void Jump::Initialize(Scope *scope) {
  scope_ = scope;
  set_body_with_parent(scope);
  if (state_.get()) { state_->Initialize(body_scope()); }
  for (auto &param : params_) { param.value->Initialize(body_scope()); }
  SetAllScopes(&stmts_, body_scope());
}

void Label::Initialize(Scope *scope) { scope_ = scope; }

void ReturnStmt::Initialize(Scope *scope) {
  scope_ = scope;
  SetAllScopes(&exprs_, scope);
}

void YieldStmt::Initialize(Scope *scope) {
  scope_ = scope;
  SetAllScopes(&exprs_, scope);
}

void ScopeLiteral::Initialize(Scope *scope) {
  scope_ = scope;
  set_body_with_parent(scope, this);
  if (state_type_) { state_type_->Initialize(scope); }
  for (auto &decl : decls_) { decl.Initialize(body_scope()); }
}

void ScopeNode::Initialize(Scope *scope) {
  scope_ = scope;
  name_->Initialize(scope);
  args_.Apply([scope](Expression *expr) { expr->Initialize(scope); });

  for (auto &block : blocks_) { block.Initialize(scope); }
}

void SliceType::Initialize(Scope *scope) {
  scope_ = scope;
  data_type_->Initialize(scope);
}

void ShortFunctionLiteral::Initialize(Scope *scope) {
  scope_ = scope;
  set_body_with_parent(scope);
  for (auto &param : params_) { param.value->Initialize(body_scope()); }
  body_->Initialize(body_scope());
  ordered_dependency_nodes_ = OrderedDependencyNodes(this);
}

void StructLiteral::Initialize(Scope *scope) {
  scope_ = scope;
  set_body_with_parent(scope);
  for (auto &field : fields_) { field.Initialize(body_scope()); }
}

void ParameterizedStructLiteral::Initialize(Scope *scope) {
  scope_ = scope;
  set_body_with_parent(scope);
  for (auto &param : params_) { param.value->Initialize(body_scope()); }
  for (auto &field : fields_) { field.Initialize(body_scope()); }
  ordered_dependency_nodes_ = OrderedDependencyNodes(this);
}

void Terminal::Initialize(Scope *scope) { scope_ = scope; }

void UnaryOperator::Initialize(Scope *scope) {
  scope_ = scope;
  operand_->Initialize(scope);
}

}  // namespace ast
