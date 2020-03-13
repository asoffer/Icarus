#include "ast/build_param_dependency_graph.h"

#include "ast/ast.h"
#include "ast/visitor.h"

namespace ast {

struct ParamDependencyGraphBuilder : Visitor<void(DependencyNode)> {
  explicit ParamDependencyGraphBuilder(
      core::Params<std::unique_ptr<Declaration>> const &params) {
    for (auto const &param : params) {
      relevant_decls_.emplace(param.value->id(), param.value.get());
    }
  }

  base::Graph<DependencyNode> Visit() && {
    for (auto [id, decl] : relevant_decls_) {
      graph_.add_edge(DependencyNode::MakeValue(decl),
                      DependencyNode::MakeType(decl));
      graph_.add_edge(DependencyNode::MakeArgValue(decl),
                      DependencyNode::MakeArgType(decl));
      if (decl->flags() & Declaration::f_IsConst) {
        graph_.add_edge(DependencyNode::MakeValue(decl),
                        DependencyNode::MakeArgValue(decl));
      }
    }

    for (auto [id, decl] : relevant_decls_) {
      if (decl->type_expr()) {
        Visit(decl->type_expr(), DependencyNode::MakeType(decl));
      }
    }
    return std::move(graph_);
  }

  void Visit(Node const *node, DependencyNode d) {
    Visitor<void(DependencyNode)>::Visit(node, d);
  }

  void Visit(Access const *node, DependencyNode d) {
    Visit(node->operand(), d);
  }

  void Visit(ArrayLiteral const *node, DependencyNode d) {
    for (auto const *expr : node->elems()) { Visit(expr, d); }
  }

  void Visit(ArrayType const *node, DependencyNode d) {
    for (auto const &len : node->lengths()) { Visit(len, d); }
    Visit(node->data_type(), d);
  }

  void Visit(Binop const *node, DependencyNode d) {
    Visit(node->lhs(), d);
    Visit(node->rhs(), d);
  }

  void Visit(BlockLiteral const *node, DependencyNode d) {
    for (auto const *b : node->before()) { Visit(b, d); }
    for (auto const *a : node->after()) { Visit(a, d); }
  }

  void Visit(BlockNode const *node, DependencyNode d) {
    for (auto const &p : node->params()) { Visit(p.value.get(), d); }
    for (auto const *stmt : node->stmts()) { Visit(stmt, d); }
  }

  void Visit(BuiltinFn const *node, DependencyNode d) {}

  void Visit(Call const *node, DependencyNode d) {
    Visit(node->callee(), d);
    for (Expression const *expr : node->args()) { Visit(expr, d); }
  }

  void Visit(Cast const *node, DependencyNode d) {
    Visit(node->expr(), d);
    Visit(node->type(), d);
  }

  void Visit(ChainOp const *node, DependencyNode d) {
    for (auto const *expr : node->exprs()) { Visit(expr, d); }
  }

  void Visit(CommaList const *node, DependencyNode d) {
    for (auto const &expr : node->exprs_) { Visit(expr.get(), d); }
  }

  void Visit(Declaration const *node, DependencyNode d) {
    if (node->type_expr()) { Visit(node->type_expr(), d); }
    if (node->init_val()) { Visit(node->init_val(), d); }
  }

  void Visit(DesignatedInitializer const *node, DependencyNode d) {
    Visit(node->type(), d);
    for (auto &[field, expr] : node->assignments()) { Visit(expr.get(), d); }
  }

  void Visit(EnumLiteral const *node, DependencyNode d) {
    for (auto const *elem : node->elems()) { Visit(elem, d); }
  }

  void Visit(FunctionLiteral const *node, DependencyNode d) {
    for (auto const &param : node->params()) { Visit(param.value.get(), d); }
    if (auto outputs = node->outputs()) {
      for (auto const &out : *outputs) { Visit(out, d); }
    }
  }

  void Visit(Identifier const *node, DependencyNode d) {
    if (auto iter = relevant_decls_.find(node->token());
        iter != relevant_decls_.end()) {
      graph_.add_edge(d, DependencyNode::MakeValue(iter->second));
    }
  }

  void Visit(Import const *node, DependencyNode d) {
    Visit(node->operand(), d);
  }

  void Visit(Index const *node, DependencyNode d) {
    Visit(node->lhs(), d);
    Visit(node->rhs(), d);
  }

  void Visit(Goto const *node, DependencyNode d) {
    for (auto const &opt : node->options()) {
      for (std::unique_ptr<Expression> const &expr : opt.args()) {
        Visit(expr.get(), d);
      }
    }
  }

  void Visit(Jump const *node, DependencyNode d) {
    if (node->state()) { Visit(node->state(), d); }
    for (auto const &param : node->params()) { Visit(param.value.get(), d); }
    for (auto const *stmt : node->stmts()) { Visit(stmt, d); }
  }

  void Visit(Label const *node, DependencyNode d) {}

  void Visit(ReturnStmt const *node, DependencyNode d) {
    for (auto *expr : node->exprs()) { Visit(expr, d); }
  }

  void Visit(YieldStmt const *node, DependencyNode d) {
    for (auto *expr : node->exprs()) { Visit(expr, d); }
  }

  void Visit(ScopeLiteral const *node, DependencyNode d) {
    for (auto const *decl : node->decls()) { Visit(decl, d); }
  }

  void Visit(ScopeNode const *node, DependencyNode d) {
    Visit(node->name(), d);
    for (Expression const *expr : node->args()) { Visit(expr, d); }
    for (auto const &block : node->blocks()) { Visit(&block, d); }
  }

  void Visit(StructLiteral const *node, DependencyNode d) {
    for (auto const &f : node->fields()) { Visit(&f, d); }
  }

  void Visit(ParameterizedStructLiteral const *node, DependencyNode d) {
    for (auto const &p : node->params()) { Visit(&p, d); }
    for (auto const &f : node->fields()) { Visit(&f, d); }
  }

  void Visit(StructType const *node, DependencyNode d) {
    for (auto &arg : node->args_) { Visit(arg.get(), d); }
  }

  void Visit(Switch const *node, DependencyNode d) {
    if (node->expr()) { Visit(node->expr(), d); }
    for (auto &[body, cond] : node->cases()) {
      Visit(body.get(), d);
      Visit(cond.get(), d);
    }
  }

  void Visit(Terminal const *node, DependencyNode d) {}

  void Visit(Unop const *node, DependencyNode d) { Visit(node->operand(), d); }

 private:
  base::Graph<DependencyNode> graph_;
  absl::flat_hash_map<std::string_view, Declaration const *> relevant_decls_;
};

base::Graph<DependencyNode> BuildParamDependencyGraph(
    core::Params<std::unique_ptr<Declaration>> const &params) {
  return ParamDependencyGraphBuilder(params).Visit();
  return base::Graph<DependencyNode>{};
}

}  // namespace ast
