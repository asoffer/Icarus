#include "module/assign_scope.h"

#include "ast/ast.h"
#include "ast/scope/decl.h"

namespace module {

template <typename T>
void SetAllScopes(AssignScope *visitor, base::PtrSpan<T> span,
                  ast::Scope *scope) {
  for (auto *n : span) { n->assign_scope(visitor, scope); }
}

void AssignScope::operator()(ast::Access *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->operand()->assign_scope(this, scope);
}

void AssignScope::operator()(ast::ArrayLiteral *node, ast::Scope *scope) {
  node->scope_ = scope;
  for (auto *expr : node->elems()) { expr->assign_scope(this, scope); }
}

void AssignScope::operator()(ast::ArrayType *node, ast::Scope *scope) {
  node->scope_ = scope;
  for (auto const &len : node->lengths()) { len->assign_scope(this, scope); }
  node->data_type()->assign_scope(this, scope);
}

void AssignScope::operator()(ast::Binop *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->lhs()->assign_scope(this, scope);
  node->rhs()->assign_scope(this, scope);
}

void AssignScope::operator()(ast::BlockLiteral *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope);
  SetAllScopes(this, node->before(), node->body_scope());
  SetAllScopes(this, node->after(), node->body_scope());
}

void AssignScope::operator()(ast::BlockNode *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope);
  SetAllScopes(this, node->args(), node->body_scope());
  SetAllScopes(this, node->stmts(), node->body_scope());
}

void AssignScope::operator()(ast::BuiltinFn *node, ast::Scope *scope) {
  node->scope_ = scope;
}

void AssignScope::operator()(ast::Call *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->callee()->assign_scope(this, scope);
  node->Apply([this, scope](ast::Expression *expr) {
    expr->assign_scope(this, scope);
  });
}

void AssignScope::operator()(ast::Cast *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->expr()->assign_scope(this, scope);
  node->type()->assign_scope(this, scope);
}

void AssignScope::operator()(ast::ChainOp *node, ast::Scope *scope) {
  node->scope_ = scope;
  for (auto *expr : node->exprs()) { expr->assign_scope(this, scope); }
}

void AssignScope::operator()(ast::CommaList *node, ast::Scope *scope) {
  node->scope_ = scope;
  for (auto &expr : node->exprs_) { expr->assign_scope(this, scope); }
}

void AssignScope::operator()(ast::Declaration *node, ast::Scope *scope) {
  ASSERT(scope != nullptr);
  node->scope_ = scope;
  node->scope_->InsertDecl(std::string{node->id()}, node);
  if (node->type_expr()) { node->type_expr()->assign_scope(this, scope); }
  if (node->init_val()) { node->init_val()->assign_scope(this, scope); }
}

void AssignScope::operator()(ast::EnumLiteral *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope);
  SetAllScopes(this, node->elems(), node->body_scope());
}

void AssignScope::operator()(ast::FunctionLiteral *node, ast::Scope *scope) {
  node->scope_             = scope;
  node->fn_scope_          = scope->add_child<ast::FnScope>();
  node->fn_scope_->fn_lit_ = node;

  for (auto &in : node->inputs_) {
    in.value->assign_scope(this, node->fn_scope_.get());
  }
  if (node->outputs_) {
    for (auto &out : *node->outputs_) {
      out->assign_scope(this, node->fn_scope_.get());
    }
  }
  SetAllScopes(this, base::PtrSpan<ast::Node>{node->statements_},
               node->fn_scope_.get());

  DependentDecls visitor;
  for (auto const &in : node->inputs_) {
    visitor.decl_graph_.graph_.add_node(in.value.get());
    if (in.value->type_expr()) {
      in.value->type_expr()->DependentDecls(&visitor, in.value.get());
    }
    if (in.value->init_val()) {
      in.value->init_val()->DependentDecls(&visitor, in.value.get());
    }
  }

  absl::flat_hash_map<std::string_view, ast::Declaration *> decls_by_id;
  for (auto const &param : node->inputs_) {
    decls_by_id.emplace(param.value->id(), param.value.get());
  }

  node->param_dep_graph_ = std::move(visitor.decl_graph_.graph_);
  for (auto &[id, decls] : visitor.decl_graph_.ids_) {
    auto iter = decls_by_id.find(id);
    if (iter == decls_by_id.end()) { continue; }
    for (auto *d : decls) { node->param_dep_graph_.add_edge(d, iter->second); }
  }

  node->sorted_params_.reserve(node->param_dep_graph_.num_nodes());
  node->param_dep_graph_.topologically([node](ast::Declaration const *decl) {
    node->sorted_params_.push_back(decl);
  });
  for (size_t i = 0; i < node->inputs_.size(); ++i) {
    node->decl_to_param_.emplace(node->inputs_.at(i).value.get(), i);
  }
}

void AssignScope::operator()(ast::Identifier *node, ast::Scope *scope) {
  node->scope_ = scope;
}

void AssignScope::operator()(ast::Import *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->operand()->assign_scope(this, scope);
}

void AssignScope::operator()(ast::Index *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->lhs()->assign_scope(this, scope);
  node->rhs()->assign_scope(this, scope);
}

void AssignScope::operator()(ast::Jump *node, ast::Scope *scope) {
  node->scope_ = scope;
  for (auto &opt : node->options_) {
    opt.args.Apply(
        [this, scope](auto &expr) { expr->assign_scope(this, scope); });
  }
}

void AssignScope::operator()(ast::JumpHandler *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope);
  SetAllScopes(this, node->input(), node->body_scope());
  SetAllScopes(this, node->stmts(), node->body_scope());
}

void AssignScope::operator()(ast::PrintStmt *node, ast::Scope *scope) {
  node->scope_ = scope;
  SetAllScopes(this, node->exprs(), scope);
}

void AssignScope::operator()(ast::ReturnStmt *node, ast::Scope *scope) {
  node->scope_ = scope;
  SetAllScopes(this, node->exprs(), scope);
}

void AssignScope::operator()(ast::YieldStmt *node, ast::Scope *scope) {
  node->scope_ = scope;
  SetAllScopes(this, node->exprs(), scope);
}

void AssignScope::operator()(ast::ScopeLiteral *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->set_body_with_parent(scope, node);
  for (auto *decl : node->decls()) {
    decl->assign_scope(this, node->body_scope());
  }
}

void AssignScope::operator()(ast::ScopeNode *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->name()->assign_scope(this, scope);
  node->Apply([this, scope](ast::Expression *expr) {
    expr->assign_scope(this, scope);
  });

  for (auto &block : node->blocks()) { block.assign_scope(this, scope); }
}

void AssignScope::operator()(ast::StructLiteral *node, ast::Scope *scope) {
  node->scope_     = scope;
  node->type_scope = scope->add_child<ast::DeclScope>();
  for (auto &a : node->args_) { a.assign_scope(this, node->type_scope.get()); }
  for (auto &f : node->fields_) {
    f.assign_scope(this, node->type_scope.get());
  }
}

void AssignScope::operator()(ast::StructType *node, ast::Scope *scope) {
  for (auto &arg : node->args_) { arg->assign_scope(this, scope); }
}

void AssignScope::operator()(ast::Switch *node, ast::Scope *scope) {
  node->scope_ = scope;
  if (node->expr_) { node->expr_->assign_scope(this, scope); }
  for (auto &[body, cond] : node->cases_) {
    body->assign_scope(this, scope);
    cond->assign_scope(this, scope);
  }
}

void AssignScope::operator()(ast::Terminal *node, ast::Scope *scope) {
  node->scope_ = scope;
}

void AssignScope::operator()(ast::Unop *node, ast::Scope *scope) {
  node->scope_ = scope;
  node->operand()->assign_scope(this, scope);
}

}  // namespace module
