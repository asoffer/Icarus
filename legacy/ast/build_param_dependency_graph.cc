#include "ast/build_param_dependency_graph.h"

#include "absl/container/node_hash_map.h"
#include "ast/ast.h"
#include "ast/module.h"

namespace ast {
namespace {

struct ParamDependencyGraphBuilder {
  using signature = void(core::DependencyNode<Declaration const *>);
  explicit ParamDependencyGraphBuilder(
      core::Parameters<Declaration> const &parameters) {
    to_process_.reserve(parameters.size());
    for (auto const &parameter : parameters) {
      // Parameters must not be multiple-declarations.
      NTH_ASSERT(parameter.value.ids().size() == 1u);
      to_process_.push_back(&parameter.value);
      relevant_decls_.emplace(parameter.value.ids()[0].name(),
                              &parameter.value);
    }
  }

  base::Graph<core::DependencyNode<Declaration>> Visit() && {
    for (auto [id, decl] : relevant_decls_) {
      graph_.add_edge(core::DependencyNode<Declaration>::ParameterValue(decl),
                      core::DependencyNode<Declaration>::ParameterType(decl));
      graph_.add_edge(core::DependencyNode<Declaration>::ArgumentValue(decl),
                      core::DependencyNode<Declaration>::ArgumentType(decl));
      graph_.add_edge(core::DependencyNode<Declaration>::ParameterType(decl),
                      core::DependencyNode<Declaration>::ArgumentType(decl));
      if (decl->flags() & Declaration::f_IsConst) {
        graph_.add_edge(core::DependencyNode<Declaration>::ParameterValue(decl),
                        core::DependencyNode<Declaration>::ArgumentValue(decl));
      }
    }

    for (auto const *decl : to_process_) {
      if (auto const *n = decl->type_expr()) {
        (*this)(n, core::DependencyNode<Declaration const *>::ParameterType(
                       &relevant_decls_.at(decl->ids()[0].name())));
      } else {
        graph_.add_edge(core::DependencyNode<Declaration>::ParameterType(decl),
                        core::DependencyNode<Declaration>::ArgumentType(decl));
      }

      if (auto const *n = decl->init_val()) {
        (*this)(n, core::DependencyNode<Declaration const *>::ParameterValue(
                       &relevant_decls_.at(decl->ids()[0].name())));
      }
    }

    for (auto [head, tail] : edges_) {
      if (*head.node() and *tail.node()) {
        graph_.add_edge(
            core::DependencyNode<Declaration>(*head.node(), head.kind()),
            core::DependencyNode<Declaration>(*tail.node(), tail.kind()));
      }
    }

    return std::move(graph_);
  }

  void operator()(BindingDeclaration const *node,
                  core::DependencyNode<Declaration const *> d) {
    auto [iter, inserted] =
        relevant_decls_.try_emplace(node->ids()[0].name(), node);
    edges_.emplace_back(
        d, core::DependencyNode<Declaration const *>::ParameterValue(
               &iter->second));
    edges_.emplace_back(
        core::DependencyNode<Declaration const *>::ParameterValue(
            &iter->second),
        core::DependencyNode<Declaration const *>::ParameterType(
            &iter->second));

    if (auto const *c = node->constraint()) {
      (*this)(c, core::DependencyNode<Declaration const *>::ParameterValue(
                     &iter->second));
    }
  }

  void operator()(Node const *node,
                  core::DependencyNode<Declaration const *> d) {
    node->visit<ParamDependencyGraphBuilder>(*this, d);
  }

  void operator()(Access const *node,
                  core::DependencyNode<Declaration const *> d) {
    (*this)(node->operand(), d);
  }

  void operator()(ArgumentType const *node,
                  core::DependencyNode<Declaration const *> d) {
    auto [iter, inserted] = relevant_decls_.try_emplace(node->name());
    edges_.emplace_back(
        d,
        core::DependencyNode<Declaration const *>::ArgumentType(&iter->second));
  }

  void operator()(ArrayLiteral const *node,
                  core::DependencyNode<Declaration const *> d) {
    for (auto const *expr : node->elements()) { (*this)(expr, d); }
  }

  void operator()(ArrayType const *node,
                  core::DependencyNode<Declaration const *> d) {
    for (auto const &len : node->lengths()) { (*this)(len, d); }
    (*this)(&node->data_type(), d);
  }

  void operator()(Assignment const *node,
                  core::DependencyNode<Declaration const *> d) {
    for (auto const *l : (node->lhs())) { (*this)(l, d); }
    for (auto const *r : (node->rhs())) { (*this)(r, d); }
  }

  void operator()(BinaryOperator const *node,
                  core::DependencyNode<Declaration const *> d) {
    (*this)(&node->lhs(), d);
    (*this)(&node->rhs(), d);
  }

  void operator()(BlockNode const *node,
                  core::DependencyNode<Declaration const *> d) {
    for (auto const &p : node->parameters()) { (*this)(&p.value, d); }
    for (auto const *stmt : node->stmts()) { (*this)(stmt, d); }
  }

  void operator()(Call const *node,
                  core::DependencyNode<Declaration const *> d) {
    (*this)(node->callee(), d);
    for (auto const &arg : node->arguments()) { (*this)(&arg.expr(), d); }
  }

  void operator()(Cast const *node,
                  core::DependencyNode<Declaration const *> d) {
    (*this)(node->expr(), d);
    (*this)(node->type(), d);
  }

  void operator()(ComparisonOperator const *node,
                  core::DependencyNode<Declaration const *> d) {
    for (auto const *expr : node->exprs()) { (*this)(expr, d); }
  }

  void operator()(Declaration const *node,
                  core::DependencyNode<Declaration const *> d) {
    if (node->type_expr()) { (*this)(node->type_expr(), d); }
    if (node->init_val()) { (*this)(node->init_val(), d); }
  }

  void operator()(DesignatedInitializer const *node,
                  core::DependencyNode<Declaration const *> d) {
    (*this)(node->type(), d);
    for (auto const *assignment : node->assignments()) {
      (*this)(assignment, d);
    }
  }

  void operator()(EnumLiteral const *node,
                  core::DependencyNode<Declaration const *> d) {
    for (auto const &[enumerator, value] : node->specified_values()) {
      (*this)(value.get(), d);
    }
  }

  void operator()(FunctionLiteral const *node,
                  core::DependencyNode<Declaration const *> d) {
    for (auto const &p : node->parameters()) { (*this)(&p.value, d); }
    if (auto outputs = node->outputs()) {
      for (auto const &out : *outputs) { (*this)(out, d); }
    }
  }

  void operator()(FunctionType const *node,
                  core::DependencyNode<Declaration const *> d) {
    for (auto const *p : node->parameters()) { (*this)(p, d); }
    for (auto const *out : node->outputs()) { (*this)(out, d); }
  }

  void operator()(ShortFunctionLiteral const *node,
                  core::DependencyNode<Declaration const *> d) {
    for (auto const &p : node->parameters()) { (*this)(&p.value, d); }
  }

  void operator()(Identifier const *node,
                  core::DependencyNode<Declaration const *> d) {
    auto [iter, inserted] = relevant_decls_.try_emplace(node->name());
    edges_.emplace_back(
        d, core::DependencyNode<Declaration const *>::ParameterValue(
               &iter->second));
  }

  void operator()(Import const *node,
                  core::DependencyNode<Declaration const *> d) {
    (*this)(node->operand(), d);
  }

  void operator()(Index const *node,
                  core::DependencyNode<Declaration const *> d) {
    (*this)(node->lhs(), d);
    (*this)(node->rhs(), d);
  }

  void operator()(InterfaceLiteral const *node,
                  core::DependencyNode<Declaration const *> d) {
    for (auto const &[name, member] : node->members()) {
      (*this)(member.get(), d);
    }
  }

  void operator()(Module const *node,
                  core::DependencyNode<Declaration const *> d) {}

  void operator()(Label const *node,
                  core::DependencyNode<Declaration const *> d) {}

  void operator()(ReturnStmt const *node,
                  core::DependencyNode<Declaration const *> d) {
    for (auto *expr : node->exprs()) { (*this)(expr, d); }
  }

  void operator()(YieldStmt const *node,
                  core::DependencyNode<Declaration const *> d) {
    for (auto const &arg : node->arguments()) { (*this)(&arg.expr(), d); }
  }

  void operator()(ScopeLiteral const *node,
                  core::DependencyNode<Declaration const *> d) {
    for (auto const &p : node->parameters()) { (*this)(&p.value, d); }
    for (auto const *stmt : node->stmts()) { (*this)(stmt, d); }
  }

  void operator()(ScopeNode const *node,
                  core::DependencyNode<Declaration const *> d) {
    (*this)(node->name(), d);
    for (auto const &arg : node->arguments()) { (*this)(&arg.expr(), d); }
    for (auto const &block : node->blocks()) { (*this)(&block, d); }
  }

  void operator()(SliceType const *node,
                  core::DependencyNode<Declaration const *> d) {
    (*this)(&node->data_type(), d);
  }

  void operator()(StructLiteral const *node,
                  core::DependencyNode<Declaration const *> d) {
    for (auto const &f : node->fields()) { (*this)(&f, d); }
  }

  void operator()(PatternMatch const *node,
                  core::DependencyNode<Declaration const *> d) {
    NTH_ASSERT(not node->is_binary());
    (*this)(&node->pattern(), d);
  }

  void operator()(ParameterizedStructLiteral const *node,
                  core::DependencyNode<Declaration const *> d) {
    for (auto const &p : node->parameters()) { (*this)(&p.value, d); }
    for (auto const &f : node->fields()) { (*this)(&f, d); }
  }

  void operator()(Terminal const *node,
                  core::DependencyNode<Declaration const *> d) {}

  void operator()(UnaryOperator const *node,
                  core::DependencyNode<Declaration const *> d) {
    (*this)(node->operand(), d);
  }

  void operator()(IfStmt const *node,
                  core::DependencyNode<Declaration const *> d) {
    (*this)(&node->condition(), d);

    for (auto const *s : node->true_block()) { (*this)(s, d); }
    if (node->has_false_block()) {
      for (auto const *s : node->false_block()) { (*this)(s, d); }
    }
  }

  void operator()(WhileStmt const *node,
                  core::DependencyNode<Declaration const *> d) {
    (*this)(&node->condition(), d);
    for (auto const *s : node->body()) { (*this)(s, d); }
  }

 private:
  base::Graph<core::DependencyNode<Declaration>> graph_;
  std::vector<Declaration const *> to_process_;
  absl::node_hash_map<std::string_view, Declaration const *> relevant_decls_;
  std::vector<std::pair<core::DependencyNode<Declaration const *>,
                        core::DependencyNode<Declaration const *>>>
      edges_;
};

}  // namespace

base::Graph<core::DependencyNode<Declaration>> BuildParamDependencyGraph(
    core::Parameters<Declaration> const &parameters) {
  return ParamDependencyGraphBuilder(parameters).Visit();
}

}  // namespace ast
