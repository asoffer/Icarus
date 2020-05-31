#include "ast/build_param_dependency_graph.h"

#include "ast/ast.h"
#include "ast/visitor.h"

namespace ast {

struct ParamDependencyGraphBuilder
    : Visitor<void(core::DependencyNode<Declaration>)> {
  explicit ParamDependencyGraphBuilder(
      core::Params<std::unique_ptr<Declaration>> const &params) {
    for (auto const &param : params) {
      relevant_decls_.emplace(param.value->id(), param.value.get());
    }
  }

  base::Graph<core::DependencyNode<Declaration>> Visit() && {
    for (auto [id, decl] : relevant_decls_) {
      graph_.add_edge(core::DependencyNode<Declaration>::MakeValue(decl),
                      core::DependencyNode<Declaration>::MakeType(decl));
      graph_.add_edge(core::DependencyNode<Declaration>::MakeArgValue(decl),
                      core::DependencyNode<Declaration>::MakeArgType(decl));
      if (decl->flags() & Declaration::f_IsConst) {
        graph_.add_edge(core::DependencyNode<Declaration>::MakeValue(decl),
                        core::DependencyNode<Declaration>::MakeArgValue(decl));
      }
    }

    for (auto [id, decl] : relevant_decls_) {
      if (auto const *n = decl->type_expr()) {
        Visit(n, core::DependencyNode<Declaration>::MakeType(decl));
      } else {
        graph_.add_edge(core::DependencyNode<Declaration>::MakeType(decl),
                        core::DependencyNode<Declaration>::MakeArgType(decl));
      }

      if (auto const *n = decl->init_val()) {
        Visit(n, core::DependencyNode<Declaration>::MakeValue(decl));
      }
    }

    return std::move(graph_);
  }

  void Visit(Node const *node, core::DependencyNode<Declaration> d) {
    Visitor<void(core::DependencyNode<Declaration>)>::Visit(node, d);
  }

  void Visit(Access const *node, core::DependencyNode<Declaration> d) {
    Visit(node->operand(), d);
  }

  void Visit(ArgumentType const *node, core::DependencyNode<Declaration> d) {
    if (auto iter = relevant_decls_.find(node->name());
        iter != relevant_decls_.end()) {
      graph_.add_edge(
          d, core::DependencyNode<Declaration>::MakeArgType(iter->second));
    }
  }

  void Visit(ArrayLiteral const *node, core::DependencyNode<Declaration> d) {
    for (auto const *expr : node->elems()) { Visit(expr, d); }
  }

  void Visit(ArrayType const *node, core::DependencyNode<Declaration> d) {
    for (auto const &len : node->lengths()) { Visit(len, d); }
    Visit(node->data_type(), d);
  }

  void Visit(Assignment const *node, core::DependencyNode<Declaration> d) {
    for (auto const *l : (node->lhs())) { Visit(l, d); }
    for (auto const *r : (node->rhs())) { Visit(r, d); }
  }

  void Visit(BinaryOperator const *node, core::DependencyNode<Declaration> d) {
    Visit(node->lhs(), d);
    Visit(node->rhs(), d);
  }

  void Visit(BlockLiteral const *node, core::DependencyNode<Declaration> d) {
    for (auto const *b : node->before()) { Visit(b, d); }
    for (auto const *a : node->after()) { Visit(a, d); }
  }

  void Visit(BlockNode const *node, core::DependencyNode<Declaration> d) {
    for (auto const &p : node->params()) { Visit(p.value.get(), d); }
    for (auto const *stmt : node->stmts()) { Visit(stmt, d); }
  }

  void Visit(BuiltinFn const *node, core::DependencyNode<Declaration> d) {}

  void Visit(Call const *node, core::DependencyNode<Declaration> d) {
    Visit(node->callee(), d);
    for (Expression const *expr : node->args()) { Visit(expr, d); }
  }

  void Visit(Cast const *node, core::DependencyNode<Declaration> d) {
    Visit(node->expr(), d);
    Visit(node->type(), d);
  }

  void Visit(ComparisonOperator const *node,
             core::DependencyNode<Declaration> d) {
    for (auto const *expr : node->exprs()) { Visit(expr, d); }
  }

  void Visit(Declaration const *node, core::DependencyNode<Declaration> d) {
    if (node->type_expr()) { Visit(node->type_expr(), d); }
    if (node->init_val()) { Visit(node->init_val(), d); }
  }

  void Visit(DesignatedInitializer const *node,
             core::DependencyNode<Declaration> d) {
    Visit(node->type(), d);
    for (auto const *assignment : node->assignments()) { Visit(assignment, d); }
  }

  void Visit(EnumLiteral const *node, core::DependencyNode<Declaration> d) {
    for (auto const *elem : node->elems()) { Visit(elem, d); }
  }

  void Visit(FunctionLiteral const *node, core::DependencyNode<Declaration> d) {
    for (auto const &param : node->params()) { Visit(param.value.get(), d); }
    if (auto outputs = node->outputs()) {
      for (auto const &out : *outputs) { Visit(out, d); }
    }
  }

  void Visit(FunctionType const *node, core::DependencyNode<Declaration> d) {
    for (auto const *param : node->params()) { Visit(param, d); }
    for (auto const *out : node->outputs()) { Visit(out, d); }
  }

  void Visit(ShortFunctionLiteral const *node,
             core::DependencyNode<Declaration> d) {
    for (auto const &param : node->params()) { Visit(param.value.get(), d); }
  }

  void Visit(Identifier const *node, core::DependencyNode<Declaration> d) {
    if (auto iter = relevant_decls_.find(node->token());
        iter != relevant_decls_.end()) {
      graph_.add_edge(
          d, core::DependencyNode<Declaration>::MakeValue(iter->second));
    }
  }

  void Visit(Import const *node, core::DependencyNode<Declaration> d) {
    Visit(node->operand(), d);
  }

  void Visit(Index const *node, core::DependencyNode<Declaration> d) {
    Visit(node->lhs(), d);
    Visit(node->rhs(), d);
  }

  void Visit(ConditionalGoto const *node, core::DependencyNode<Declaration> d) {
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
             core::DependencyNode<Declaration> d) {
    for (auto const &opt : node->options()) {
      for (std::unique_ptr<Expression> const &expr : opt.args()) {
        Visit(expr.get(), d);
      }
    }
  }

  void Visit(Jump const *node, core::DependencyNode<Declaration> d) {
    if (node->state()) { Visit(node->state(), d); }
    for (auto const &param : node->params()) { Visit(param.value.get(), d); }
    for (auto const *stmt : node->stmts()) { Visit(stmt, d); }
  }

  void Visit(Label const *node, core::DependencyNode<Declaration> d) {}

  void Visit(ReturnStmt const *node, core::DependencyNode<Declaration> d) {
    for (auto *expr : node->exprs()) { Visit(expr, d); }
  }

  void Visit(YieldStmt const *node, core::DependencyNode<Declaration> d) {
    for (auto *expr : node->exprs()) { Visit(expr, d); }
  }

  void Visit(ScopeLiteral const *node, core::DependencyNode<Declaration> d) {
    for (auto const &decl : node->decls()) { Visit(&decl, d); }
  }

  void Visit(ScopeNode const *node, core::DependencyNode<Declaration> d) {
    Visit(node->name(), d);
    for (Expression const *expr : node->args()) { Visit(expr, d); }
    for (auto const &block : node->blocks()) { Visit(&block, d); }
  }

  void Visit(StructLiteral const *node, core::DependencyNode<Declaration> d) {
    for (auto const &f : node->fields()) { Visit(&f, d); }
  }

  void Visit(ParameterizedStructLiteral const *node,
             core::DependencyNode<Declaration> d) {
    for (auto const &param : node->params()) { Visit(param.value.get(), d); }
    for (auto const &f : node->fields()) { Visit(&f, d); }
  }

  void Visit(Terminal const *node, core::DependencyNode<Declaration> d) {}

  void Visit(UnaryOperator const *node, core::DependencyNode<Declaration> d) {
    Visit(node->operand(), d);
  }

 private:
  base::Graph<core::DependencyNode<Declaration>> graph_;
  absl::flat_hash_map<std::string_view, Declaration const *> relevant_decls_;
};

base::Graph<core::DependencyNode<Declaration>> BuildParamDependencyGraph(
    core::Params<std::unique_ptr<Declaration>> const &params) {
  return ParamDependencyGraphBuilder(params).Visit();
}

}  // namespace ast
