#include "ast/build_param_dependency_graph.h"

#include "absl/container/node_hash_map.h"
#include "ast/ast.h"
#include "ast/module.h"
#include "ast/visitor.h"

namespace ast {
namespace {

struct ParamDependencyGraphBuilder
    : Visitor<void(core::DependencyNode<Declaration const *>)> {
  explicit ParamDependencyGraphBuilder(
      core::Params<std::unique_ptr<Declaration>> const &params) {
    to_process_.reserve(params.size());
    for (auto const &param : params) {
      // Parameters must not be multiple-declarations.
      ASSERT(param.value->ids().size() == 1u);
      to_process_.push_back(param.value.get());
      relevant_decls_.emplace(param.value->ids()[0].name(), param.value.get());
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
        Visit(n, core::DependencyNode<Declaration const *>::ParameterType(
                     &relevant_decls_.at(decl->ids()[0].name())));
      } else {
        graph_.add_edge(core::DependencyNode<Declaration>::ParameterType(decl),
                        core::DependencyNode<Declaration>::ArgumentType(decl));
      }

      if (auto const *n = decl->init_val()) {
        Visit(n, core::DependencyNode<Declaration const *>::ParameterValue(
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

  void Visit(BindingDeclaration const *node,
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
  }

  void Visit(Node const *node, core::DependencyNode<Declaration const *> d) {
    Visitor<void(core::DependencyNode<Declaration const *>)>::Visit(node, d);
  }

  void Visit(Access const *node, core::DependencyNode<Declaration const *> d) {
    Visit(node->operand(), d);
  }

  void Visit(ArgumentType const *node,
             core::DependencyNode<Declaration const *> d) {
    auto [iter, inserted] = relevant_decls_.try_emplace(node->name());
    edges_.emplace_back(
        d,
        core::DependencyNode<Declaration const *>::ArgumentType(&iter->second));
  }

  void Visit(ArrayLiteral const *node,
             core::DependencyNode<Declaration const *> d) {
    for (auto const *expr : node->elems()) { Visit(expr, d); }
  }

  void Visit(ArrayType const *node,
             core::DependencyNode<Declaration const *> d) {
    for (auto const &len : node->lengths()) { Visit(len, d); }
    Visit(node->data_type(), d);
  }

  void Visit(Assignment const *node,
             core::DependencyNode<Declaration const *> d) {
    for (auto const *l : (node->lhs())) { Visit(l, d); }
    for (auto const *r : (node->rhs())) { Visit(r, d); }
  }

  void Visit(BinaryOperator const *node,
             core::DependencyNode<Declaration const *> d) {
    Visit(&node->lhs(), d);
    Visit(&node->rhs(), d);
  }

  void Visit(BlockLiteral const *node,
             core::DependencyNode<Declaration const *> d) {
    for (auto const *b : node->before()) { Visit(b, d); }
    for (auto const *a : node->after()) { Visit(a, d); }
  }

  void Visit(BlockNode const *node,
             core::DependencyNode<Declaration const *> d) {
    for (auto const &p : node->params()) { Visit(p.value.get(), d); }
    for (auto const *stmt : node->stmts()) { Visit(stmt, d); }
  }

  void Visit(BuiltinFn const *node,
             core::DependencyNode<Declaration const *> d) {}

  void Visit(Call const *node, core::DependencyNode<Declaration const *> d) {
    Visit(node->callee(), d);
    for (auto const &arg : node->arguments()) { Visit(&arg.expr(), d); }
  }

  void Visit(Cast const *node, core::DependencyNode<Declaration const *> d) {
    Visit(node->expr(), d);
    Visit(node->type(), d);
  }

  void Visit(ComparisonOperator const *node,
             core::DependencyNode<Declaration const *> d) {
    for (auto const *expr : node->exprs()) { Visit(expr, d); }
  }

  void Visit(Declaration const *node,
             core::DependencyNode<Declaration const *> d) {
    if (node->type_expr()) { Visit(node->type_expr(), d); }
    if (node->init_val()) { Visit(node->init_val(), d); }
  }

  void Visit(DesignatedInitializer const *node,
             core::DependencyNode<Declaration const *> d) {
    Visit(node->type(), d);
    for (auto const *assignment : node->assignments()) { Visit(assignment, d); }
  }

  void Visit(EnumLiteral const *node,
             core::DependencyNode<Declaration const *> d) {
    for (auto const &[enumerator, value] : node->specified_values()) {
      Visit(value.get(), d);
    }
  }

  void Visit(FunctionLiteral const *node,
             core::DependencyNode<Declaration const *> d) {
    for (auto const &param : node->params()) { Visit(param.value.get(), d); }
    if (auto outputs = node->outputs()) {
      for (auto const &out : *outputs) { Visit(out, d); }
    }
  }

  void Visit(FunctionType const *node,
             core::DependencyNode<Declaration const *> d) {
    for (auto const *param : node->params()) { Visit(param, d); }
    for (auto const *out : node->outputs()) { Visit(out, d); }
  }

  void Visit(ShortFunctionLiteral const *node,
             core::DependencyNode<Declaration const *> d) {
    for (auto const &param : node->params()) { Visit(param.value.get(), d); }
  }

  void Visit(Identifier const *node,
             core::DependencyNode<Declaration const *> d) {
    auto [iter, inserted] = relevant_decls_.try_emplace(node->name());
    edges_.emplace_back(
        d, core::DependencyNode<Declaration const *>::ParameterValue(
               &iter->second));
  }

  void Visit(Import const *node, core::DependencyNode<Declaration const *> d) {
    Visit(node->operand(), d);
  }

  void Visit(Index const *node, core::DependencyNode<Declaration const *> d) {
    Visit(node->lhs(), d);
    Visit(node->rhs(), d);
  }

  void Visit(InterfaceLiteral const *node,
             core::DependencyNode<Declaration const *> d) {
    for (auto &[name, expr] : node->entries()) {
      Visit(name.get(), d);
      Visit(expr.get(), d);
    }
  }

  void Visit(ConditionalGoto const *node,
             core::DependencyNode<Declaration const *> d) {
    Visit(node->condition(), d);
    for (auto const &opt : node->true_options()) {
      for (std::unique_ptr<Expression> const &expr : opt.args()) {
        Visit(expr.get(), d);
      }
    }
    for (auto const &opt : node->false_options()) {
      for (std::unique_ptr<Expression> const &expr : opt.args()) {
        Visit(expr.get(), d);
      }
    }
  }

  void Visit(UnconditionalGoto const *node,
             core::DependencyNode<Declaration const *> d) {
    for (auto const &opt : node->options()) {
      for (std::unique_ptr<Expression> const &expr : opt.args()) {
        Visit(expr.get(), d);
      }
    }
  }

  void Visit(Jump const *node, core::DependencyNode<Declaration const *> d) {
    if (node->state()) { Visit(node->state(), d); }
    for (auto const &param : node->params()) { Visit(param.value.get(), d); }
    for (auto const *stmt : node->stmts()) { Visit(stmt, d); }
  }

  void Visit(Module const *node, core::DependencyNode<Declaration const *> d) {}

  void Visit(Label const *node, core::DependencyNode<Declaration const *> d) {}

  void Visit(ReturnStmt const *node,
             core::DependencyNode<Declaration const *> d) {
    for (auto *expr : node->exprs()) { Visit(expr, d); }
  }

  void Visit(YieldStmt const *node,
             core::DependencyNode<Declaration const *> d) {
    for (auto const &arg : node->arguments()) { Visit(&arg.expr(), d); }
  }

  void Visit(ScopeLiteral const *node,
             core::DependencyNode<Declaration const *> d) {
    for (auto const &param : node->params()) { Visit(param.value.get(), d); }
    for (auto const *stmt : node->stmts()) { Visit(stmt, d); }
  }

  void Visit(ScopeNode const *node,
             core::DependencyNode<Declaration const *> d) {
    Visit(node->name(), d);
    for (auto const &arg : node->arguments()) { Visit(&arg.expr(), d); }
    for (auto const &block : node->blocks()) { Visit(&block, d); }
  }

  void Visit(SliceType const *node,
             core::DependencyNode<Declaration const *> d) {
    Visit(node->data_type(), d);
  }

  void Visit(StructLiteral const *node,
             core::DependencyNode<Declaration const *> d) {
    for (auto const &f : node->fields()) { Visit(&f, d); }
  }

  void Visit(PatternMatch const *node,
             core::DependencyNode<Declaration const *> d) {
    ASSERT(node->is_binary() == false);
    Visit(&node->pattern(), d);
  }

  void Visit(ParameterizedStructLiteral const *node,
             core::DependencyNode<Declaration const *> d) {
    for (auto const &param : node->params()) { Visit(param.value.get(), d); }
    for (auto const &f : node->fields()) { Visit(&f, d); }
  }

  void Visit(Terminal const *node,
             core::DependencyNode<Declaration const *> d) {}

  void Visit(UnaryOperator const *node,
             core::DependencyNode<Declaration const *> d) {
    Visit(node->operand(), d);
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
    core::Params<std::unique_ptr<Declaration>> const &params) {
  return ParamDependencyGraphBuilder(params).Visit();
}

}  // namespace ast
