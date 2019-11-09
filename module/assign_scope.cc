#include "module/assign_scope.h"

#include "ast/ast.h"
#include "ast/scope/decl.h"
#include "module/dependent_decls.h"

namespace module {

template <typename T>
static void SetAllScopes(AssignScope *visitor, base::PtrSpan<T> span,
                         ast::Scope *scope) {
  for (auto *n : span) { visitor->Visit(n, scope); }
}

void AssignScope::To(base::PtrSpan<ast::Node> nodes, ast::Scope *scope) {
  AssignScope visitor;
  SetAllScopes(&visitor, nodes, scope);
}

void AssignScope::Visit(ast::Access *node, ast::Scope *scope) {
  node->scope_ = scope;
  Visit(node->operand(), scope);
}

void AssignScope::Visit(ast::ArrayLiteral *node, ast::Scope *scope) {
  node->scope_ = scope;
  for (auto *expr : node->elems()) { Visit(expr, scope); }
}

void AssignScope::Visit(ast::ArrayType *node, ast::Scope *scope) {
  node->scope_ = scope;
  for (auto const &len : node->lengths()) { Visit(len, scope); }
  Visit(node->data_type(), scope);
}

void AssignScope::Visit(ast::Binop *node, ast::Scope *scope) {
  node->scope_ = scope;
  Visit(node->lhs(), scope);
  Visit(node->rhs(), scope);
}

void AssignScope::Visit(ast::BlockLiteral *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope);
  SetAllScopes(this, node->before(), node->body_scope());
  SetAllScopes(this, node->after(), node->body_scope());
}

void AssignScope::Visit(ast::BlockNode *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope);
  SetAllScopes(this, node->args(), node->body_scope());
  SetAllScopes(this, node->stmts(), node->body_scope());
}

void AssignScope::Visit(ast::BuiltinFn *node, ast::Scope *scope) {
  node->scope_ = scope;
}

void AssignScope::Visit(ast::Call *node, ast::Scope *scope) {
  node->scope_ = scope;
  Visit(node->callee(), scope);
  node->Apply([this, scope](ast::Expression *expr) { Visit(expr, scope); });
}

void AssignScope::Visit(ast::Cast *node, ast::Scope *scope) {
  node->scope_ = scope;
  Visit(node->expr(), scope);
  Visit(node->type(), scope);
}

void AssignScope::Visit(ast::ChainOp *node, ast::Scope *scope) {
  node->scope_ = scope;
  for (auto *expr : node->exprs()) { Visit(expr, scope); }
}

void AssignScope::Visit(ast::CommaList *node, ast::Scope *scope) {
  node->scope_ = scope;
  for (auto &expr : node->exprs_) { Visit(expr.get(), scope); }
}

void AssignScope::Visit(ast::Declaration *node, ast::Scope *scope) {
  ASSERT(scope != nullptr);
  node->scope_ = scope;
  node->scope_->InsertDecl(node->id(), node);
  if (node->type_expr()) { Visit(node->type_expr(), scope); }
  if (node->init_val()) { Visit(node->init_val(), scope); }
}

void AssignScope::Visit(ast::EnumLiteral *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope);
  SetAllScopes(this, node->elems(), node->body_scope());
}

void AssignScope::Visit(ast::FunctionLiteral *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope, node);

  for (auto &param : node->params()) { Visit(param.value.get(), node->body_scope()); }
  if (auto outputs = node->outputs()) {
    for (auto *out : *outputs) { Visit(out, node->body_scope()); }
  }
  SetAllScopes(this, node->stmts(), node->body_scope());

  DependentDecls dep_decls;
  for (auto const &param : node->params()) {
    dep_decls.decl_graph_.graph_.add_node(param.value.get());
    if (param.value->type_expr()) {
      dep_decls.Visit(param.value->type_expr(), param.value.get());
    }
    if (param.value->init_val()) {
      dep_decls.Visit(param.value->init_val(), param.value.get());
    }
  }

  absl::flat_hash_map<std::string_view, ast::Declaration *> decls_by_id;
  for (auto const &param : node->params()) {
    decls_by_id.emplace(param.value->id(), param.value.get());
  }

  node->param_dep_graph_ = std::move(dep_decls.decl_graph_.graph_);
  for (auto &[id, decls] : dep_decls.decl_graph_.ids_) {
    auto iter = decls_by_id.find(id);
    if (iter == decls_by_id.end()) { continue; }
    for (auto *d : decls) { node->param_dep_graph_.add_edge(d, iter->second); }
  }

  node->sorted_params_.reserve(node->param_dep_graph_.num_nodes());
  node->param_dep_graph_.topologically([node](ast::Declaration const *decl) {
    node->sorted_params_.push_back(decl);
  });

  size_t i = 0;
  for (auto const &param : node->params()) {
    node->decl_to_param_.emplace(param.value.get(), i++);
  }
}

void AssignScope::Visit(ast::Identifier *node, ast::Scope *scope) {
  node->scope_ = scope;
}

void AssignScope::Visit(ast::Import *node, ast::Scope *scope) {
  node->scope_ = scope;
  Visit(node->operand(), scope);
}

void AssignScope::Visit(ast::Index *node, ast::Scope *scope) {
  node->scope_ = scope;
  Visit(node->lhs(), scope);
  Visit(node->rhs(), scope);
}

void AssignScope::Visit(ast::Jump *node, ast::Scope *scope) {
  node->scope_ = scope;
  for (auto &opt : node->options_) {
    opt.args.Apply([this, scope](auto &expr) { Visit(expr.get(), scope); });
  }
}

void AssignScope::Visit(ast::JumpHandler *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope);
  SetAllScopes(this, node->input(), node->body_scope());
  SetAllScopes(this, node->stmts(), node->body_scope());
}

void AssignScope::Visit(ast::PrintStmt *node, ast::Scope *scope) {
  node->scope_ = scope;
  SetAllScopes(this, node->exprs(), scope);
}

void AssignScope::Visit(ast::ReturnStmt *node, ast::Scope *scope) {
  node->scope_ = scope;
  SetAllScopes(this, node->exprs(), scope);
}

void AssignScope::Visit(ast::YieldStmt *node, ast::Scope *scope) {
  node->scope_ = scope;
  SetAllScopes(this, node->exprs(), scope);
}

void AssignScope::Visit(ast::ScopeLiteral *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope, node);
  for (auto *decl : node->decls()) { Visit(decl, node->body_scope()); }
}

void AssignScope::Visit(ast::ScopeNode *node, ast::Scope *scope) {
  node->scope_ = scope;
  Visit(node->name(), scope);
  node->Apply([this, scope](ast::Expression *expr) { Visit(expr, scope); });

  for (auto &block : node->blocks()) { Visit(&block, scope); }
}

void AssignScope::Visit(ast::StructLiteral *node, ast::Scope *scope) {
  node->scope_     = scope;
  node->type_scope = scope->add_child<ast::DeclScope>();
  for (auto &a : node->args_) { Visit(&a, node->type_scope.get()); }
  for (auto &f : node->fields_) { Visit(&f, node->type_scope.get()); }
}

void AssignScope::Visit(ast::StructType *node, ast::Scope *scope) {
  for (auto &arg : node->args_) { Visit(arg.get(), scope); }
}

void AssignScope::Visit(ast::Switch *node, ast::Scope *scope) {
  node->scope_ = scope;
  if (node->expr_) { Visit(node->expr_.get(), scope); }
  for (auto &[body, cond] : node->cases_) {
    Visit(body.get(), scope);
    Visit(cond.get(), scope);
  }
}

void AssignScope::Visit(ast::Terminal *node, ast::Scope *scope) {
  node->scope_ = scope;
}

void AssignScope::Visit(ast::Unop *node, ast::Scope *scope) {
  node->scope_ = scope;
  Visit(node->operand(), scope);
}

}  // namespace module
