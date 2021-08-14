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

void ParameterizedExpression::InitializeParams() {
  for (auto& param : params_) {
    param.value->flags() |= Declaration::f_IsFnParam;
    if (not param.value->IsDefaultInitialized()) {
      param.flags = core::HAS_DEFAULT;
    }
    if (not is_generic_) {
      is_generic_ = (param.value->flags() & Declaration::f_IsConst) or
                    param.value->is_dependent();
    }
  }
}

template <typename T>
static void InitializeAll(std::vector<std::unique_ptr<T>>& nodes,
                          Node::Initializer& initializer, bool* covers,
                          bool* dep) {
  for (auto& n : nodes) {
    n->Initialize(initializer);
    *covers |= n->covers_binding();
    *dep |= n->is_dependent();
  }
}

void InitializeNodes(base::PtrSpan<Node> nodes,
                     Node::Initializer& initializer) {
  for (auto* n : nodes) { n->Initialize(initializer); }
}

void Access::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  operand_->Initialize(initializer);
  covers_binding_ = operand_->covers_binding();
  is_dependent_   = operand_->is_dependent();
}

void ArgumentType::Initialize(Initializer& initializer) {
  scope_        = initializer.scope;
  is_dependent_ = true;
}

void ArrayLiteral::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  for (auto& expr : elems_) {
    expr->Initialize(initializer);
    covers_binding_ |= expr->covers_binding();
    is_dependent_ |= expr->is_dependent();
  }
}

void ArrayType::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  for (auto const& len : lengths_) {
    len->Initialize(initializer);
    covers_binding_ |= len->covers_binding();
    is_dependent_ |= len->is_dependent();
  }
  data_type_->Initialize(initializer);
  covers_binding_ |= data_type_->covers_binding();
  is_dependent_ |= data_type_->is_dependent();
}

void Assignment::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  InitializeAll(lhs_, initializer, &covers_binding_, &is_dependent_);
  InitializeAll(rhs_, initializer, &covers_binding_, &is_dependent_);
}

void BinaryOperator::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  lhs_->Initialize(initializer);
  rhs_->Initialize(initializer);
  covers_binding_ = lhs_->covers_binding() or rhs_->covers_binding();
  is_dependent_   = lhs_->is_dependent() or rhs_->is_dependent();
}

void BindingDeclaration::Initialize(Initializer& initializer) {
  Declaration::Initialize(initializer);
  covers_binding_ = true;
  is_dependent_   = true;
  pattern_        = initializer.pattern;
}

void BlockLiteral::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  set_body_with_parent(initializer.scope);
  initializer.scope = &body_scope();
  absl::Cleanup c   = [&] { initializer.scope = scope_; };
  InitializeAll(before_, initializer, &covers_binding_, &is_dependent_);
  InitializeAll(after_, initializer, &covers_binding_, &is_dependent_);
}

void BlockNode::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  set_body_with_parent(initializer.scope, true);
  initializer.scope = &body_scope();
  absl::Cleanup c   = [&] { initializer.scope = scope_; };
  for (auto& param : params_) { param.value->Initialize(initializer); }
  InitializeAll(stmts_, initializer, &covers_binding_, &is_dependent_);
  InitializeParams();
}

void BuiltinFn::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
}

void Call::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  callee_->Initialize(initializer);
  covers_binding_ = callee_->covers_binding();
  is_dependent_   = callee_->is_dependent();
  for (auto& arg : arguments_) {
    arg.expr().Initialize(initializer);
    covers_binding_ |= arg.expr().covers_binding();
    is_dependent_ |= arg.expr().is_dependent();
  }
}

void Cast::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  expr_->Initialize(initializer);
  type_->Initialize(initializer);
  covers_binding_ = expr_->covers_binding() or type_->covers_binding();
  is_dependent_   = expr_->is_dependent() or type_->is_dependent();
}

void ComparisonOperator::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  for (auto& expr : exprs_) {
    expr->Initialize(initializer);
    covers_binding_ |= expr->covers_binding();
    is_dependent_ |= expr->is_dependent();
  }
}

void Declaration::Initialize(Initializer& initializer) {
  scope_ = ASSERT_NOT_NULL(initializer.scope);
  scope_->InsertDeclaration(this);
  if (type_expr_) {
    auto* m         = std::exchange(initializer.match_against, this);
    absl::Cleanup c = [&] { initializer.match_against = m; };
    type_expr_->Initialize(initializer);
    covers_binding_ |= type_expr_->covers_binding();
    is_dependent_ |= type_expr_->is_dependent();
  }
  if (init_val_) {
    init_val_->Initialize(initializer);
    covers_binding_ |= init_val_->covers_binding();
    is_dependent_ |= init_val_->is_dependent();
  }
  for (auto& id : ids_) {
    id.Initialize(initializer);
    covers_binding_ |= id.covers_binding();
    is_dependent_ |= id.is_dependent();
  }
}

void DesignatedInitializer::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  type_->Initialize(initializer);
  for (auto& assignment : assignments_) {
    assignment->Initialize(initializer);
    covers_binding_ |= assignment->covers_binding();
    is_dependent_ |= assignment->is_dependent();
  }
}

void EnumLiteral::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  set_body_with_parent(initializer.scope);
  for (auto& [id, value] : values_) {
    value->Initialize(initializer);
    covers_binding_ |= value->covers_binding();
    is_dependent_ |= value->is_dependent();
  }
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
  for (auto& param : params_) {
    param.value->Initialize(initializer);
    covers_binding_ |= param.value->covers_binding();
    is_dependent_ |= param.value->is_dependent();
  }
  if (outputs_) {
    for (auto& out : *outputs_) { out->Initialize(initializer); }
  }
  InitializeAll(stmts_, initializer, &covers_binding_, &is_dependent_);
  ordered_dependency_nodes_ = OrderedDependencyNodes(this);
  InitializeParams();
}

void FunctionType::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  InitializeAll(params_, initializer, &covers_binding_, &is_dependent_);
  InitializeAll(output_, initializer, &covers_binding_, &is_dependent_);
}

void Identifier::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
}

void Import::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  operand_->Initialize(initializer);
  covers_binding_ = operand_->covers_binding();
  is_dependent_   = operand_->is_dependent();
}

void Index::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  lhs_->Initialize(initializer);
  rhs_->Initialize(initializer);
  covers_binding_ = lhs_->covers_binding() or rhs_->covers_binding();
  is_dependent_   = lhs_->is_dependent() or rhs_->is_dependent();
}

void InterfaceLiteral::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  set_body_with_parent(initializer.scope);
  initializer.scope = &body_scope();
  absl::Cleanup c   = [&] { initializer.scope = scope_; };
  for (auto& [name, expr] : entries_) {
    name->Initialize(initializer);
    expr->Initialize(initializer);
    covers_binding_ |= name->covers_binding() or expr->covers_binding();
    is_dependent_ |= name->is_dependent() or expr->is_dependent();
  }
}

void ConditionalGoto::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  condition_->Initialize(initializer);
  for (auto& opt : true_options_) {
    opt.args_.Apply([&](auto& expr) {
      expr->Initialize(initializer);
      covers_binding_ |= expr->covers_binding();
      is_dependent_ |= expr->is_dependent();
    });
  }
  for (auto& opt : false_options_) {
    opt.args_.Apply([&](auto& expr) {
      expr->Initialize(initializer);
      covers_binding_ |= expr->covers_binding();
      is_dependent_ |= expr->is_dependent();
    });
  }
}

void UnconditionalGoto::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  for (auto& opt : options_) {
    opt.args_.Apply([&](auto& expr) {
      expr->Initialize(initializer);
      covers_binding_ |= expr->covers_binding();
      is_dependent_ |= expr->is_dependent();
    });
  }
}

void Jump::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  set_body_with_parent(initializer.scope);
  initializer.scope = &body_scope();
  absl::Cleanup c   = [&] { initializer.scope = scope_; };
  if (state_) {
    state_->Initialize(initializer);
    covers_binding_ |= state_->covers_binding();
    is_dependent_ |= state_->is_dependent();
  }
  for (auto& param : params_) {
    param.value->Initialize(initializer);
    covers_binding_ |= param.value->covers_binding();
    is_dependent_ |= param.value->is_dependent();
  }
  InitializeAll(stmts_, initializer, &covers_binding_, &is_dependent_);
  InitializeParams();
}

void Label::Initialize(Initializer& initializer) { scope_ = initializer.scope; }

void ReturnStmt::Initialize(Initializer& initializer) {
  scope_            = initializer.scope;
  function_literal_ = initializer.function_literal;
  InitializeAll(exprs_, initializer, &covers_binding_, &is_dependent_);
}

void YieldStmt::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  for (auto& arg : args_) {
    arg.expr().Initialize(initializer);
    covers_binding_ |= arg.expr().covers_binding();
    is_dependent_ |= arg.expr().is_dependent();
  }
}

void ScopeLiteral::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  set_body_with_parent(initializer.scope);
  if (state_type_) { state_type_->Initialize(initializer); }
  initializer.scope = &body_scope();
  absl::Cleanup c   = [&] { initializer.scope = scope_; };
  for (auto& decl : decls_) { decl.Initialize(initializer); }
}

void ScopeNode::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  name_->Initialize(initializer);
  for (auto& arg : args_) {
    arg.expr().Initialize(initializer);
    covers_binding_ |= arg.expr().covers_binding();
    is_dependent_ |= arg.expr().is_dependent();
  }

  for (auto& block : blocks_) { block.Initialize(initializer); }
}

void SliceType::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  data_type_->Initialize(initializer);
  covers_binding_ = data_type_->covers_binding();
  is_dependent_   = data_type_->is_dependent();
}

void ShortFunctionLiteral::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  set_body_with_parent(initializer.scope);
  initializer.scope = &body_scope();
  absl::Cleanup c   = [&] { initializer.scope = scope_; };
  for (auto& param : params_) { param.value->Initialize(initializer); }
  body_->Initialize(initializer);
  ordered_dependency_nodes_ = OrderedDependencyNodes(this);
  InitializeParams();
}

void StructLiteral::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  set_body_with_parent(initializer.scope);
  initializer.scope = &body_scope();
  absl::Cleanup c   = [&] { initializer.scope = scope_; };
  for (auto& field : fields_) { field.Initialize(initializer); }
}

void ParameterizedStructLiteral::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  set_body_with_parent(initializer.scope);
  initializer.scope = &body_scope();
  absl::Cleanup c   = [&] { initializer.scope = scope_; };
  for (auto& param : params_) { param.value->Initialize(initializer); }
  for (auto& field : fields_) { field.Initialize(initializer); }
  ordered_dependency_nodes_ = OrderedDependencyNodes(this);
  InitializeParams();
}

void PatternMatch::Initialize(Initializer& initializer) {
  covers_binding_ = false;
  is_dependent_   = true;
  scope_          = initializer.scope;
  auto const* p   = std::exchange(initializer.pattern, this);
  absl::Cleanup c = [&] { initializer.pattern = p; };
  pattern_->Initialize(initializer);
  if (is_binary()) {
    expr().Initialize(initializer);
  } else {
    set_match_against(initializer.match_against);
  }
}

void Terminal::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
}

void UnaryOperator::Initialize(Initializer& initializer) {
  scope_ = initializer.scope;
  operand_->Initialize(initializer);
  covers_binding_ = operand_->covers_binding();
  is_dependent_   = operand_->is_dependent();
}

}  // namespace ast
