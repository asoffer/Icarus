#include "visitor/assign_scope.h"

#include "ast/ast.h"

namespace visitor {

void AssignScope::operator()(ast::Access *node, core::Scope *scope) {
  node->scope_ = scope;
  node->operand->assign_scope(this, scope);
}

void AssignScope::operator()(ast::ArrayLiteral *node,
                             core::Scope *scope) {
  node->cl_.assign_scope(this, scope);
}

void AssignScope::operator()(ast::ArrayType *node, core::Scope *scope) {
  node->scope_ = scope;
  node->length_->assign_scope(this, scope);
  node->data_type_->assign_scope(this, scope);
}

void AssignScope::operator()(ast::Binop *node, core::Scope *scope) {
  node->scope_ = scope;
  node->lhs->assign_scope(this, scope);
  node->rhs->assign_scope(this, scope);
}

void AssignScope::operator()(ast::BlockLiteral *node,
                             core::Scope *scope) {
  node->scope_      = scope;
  node->body_scope_ = scope->add_child<core::DeclScope>();
  for (auto &b : node->before_) { b.assign_scope(this, node->body_scope_.get()); }
  for (auto &a : node->after_) { a.assign_scope(this, node->body_scope_.get()); }
}

void AssignScope::operator()(ast::BlockNode *node, core::Scope *scope) {
  node->scope_ = scope;
  node->name_->assign_scope(this, scope);
  node->block_scope_ = scope->add_child<core::ExecScope>();
  node->stmts_.assign_scope(this, node->block_scope_.get());
}

void AssignScope::operator()(ast::BuiltinFn *node, core::Scope *scope) {
  node->scope_ = scope;
}

void AssignScope::operator()(ast::Call *node, core::Scope *scope) {
  node->scope_ = scope;
  node->fn_->assign_scope(this, scope);
  node->args_.Apply([this, scope](auto &expr) { expr->assign_scope(this, scope); });
}

void AssignScope::operator()(ast::Cast *node, core::Scope *scope) {
  node->scope_ = scope;
  node->expr_->assign_scope(this, scope);
  node->type_->assign_scope(this, scope);
}

void AssignScope::operator()(ast::ChainOp *node, core::Scope *scope) {
  node->scope_ = scope;
  for (auto &expr : node->exprs) { expr->assign_scope(this, scope); }
}

void AssignScope::operator()(ast::CommaList *node, core::Scope *scope) {
  node->scope_ = scope;
  for (auto &expr : node->exprs_) { expr->assign_scope(this, scope); }
}

void AssignScope::operator()(ast::Declaration *node, core::Scope *scope) {
  ASSERT(scope != nullptr);
  node->scope_ = scope;
  node->scope_->InsertDecl(node);
  if (node->type_expr) { node->type_expr->assign_scope(this, scope); }
  if (node->init_val) { node->init_val->assign_scope(this, scope); }
}

void AssignScope::operator()(ast::EnumLiteral *node, core::Scope *scope) {
  node->enum_scope_ = scope->add_child<core::DeclScope>();
  for (auto &elem : node->elems_) {
    elem->assign_scope(this, node->enum_scope_.get());
  }
}

void AssignScope::operator()(ast::FunctionLiteral *node,
                             core::Scope *scope) {
  node->scope_             = scope;
  node->fn_scope_          = scope->add_child<core::FnScope>();
  node->fn_scope_->fn_lit_ = node;

  for (auto &in : node->inputs_) {
    in.value->assign_scope(this, node->fn_scope_.get());
  }
  for (auto &out : node->outputs_) { out->assign_scope(this, node->fn_scope_.get()); }
  node->statements_.assign_scope(this, node->fn_scope_.get());

  DependentDecls visitor;
  for (auto const &in : node->inputs_) {
    visitor.decl_graph_.graph_.add_node(in.value.get());
    if (in.value->type_expr) {
      in.value->type_expr->DependentDecls(&visitor, in.value.get());
    }
    if (in.value->init_val) {
      in.value->init_val->DependentDecls(&visitor, in.value.get());
    }
  }

  absl::flat_hash_map<std::string_view, ast::Declaration *> decls_by_id;
  for (auto const &param : node->inputs_) {
    decls_by_id.emplace(param.value->id_, param.value.get());
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

void AssignScope::operator()(ast::Identifier *node, core::Scope *scope) {
  node->scope_ = scope;
}

void AssignScope::operator()(ast::Import *node, core::Scope *scope) {
  node->scope_ = scope;
  node->operand_->assign_scope(this, scope);
}

void AssignScope::operator()(ast::Index *node, core::Scope *scope) {
  node->scope_ = scope;
  node->lhs_->assign_scope(this, scope);
  node->rhs_->assign_scope(this, scope);
}

void AssignScope::operator()(ast::Interface *node, core::Scope *scope) {
  node->scope_      = scope;
  node->body_scope_ = scope->add_child<core::DeclScope>();
  for (auto &d : node->decls_) { d.assign_scope(this, node->body_scope_.get()); }
}

void AssignScope::operator()(ast::RepeatedUnop *node,
                             core::Scope *scope) {
  node->scope_ = scope;
  node->args_.assign_scope(this, scope);
}

void AssignScope::operator()(ast::ScopeLiteral *node,
                             core::Scope *scope) {
  node->scope_      = scope;
  node->body_scope_ = scope->add_child<core::ScopeLitScope>(node);
  for (auto &decl : node->decls_) {
    decl.assign_scope(this, node->body_scope_.get());
  }
}

void AssignScope::operator()(ast::ScopeNode *node, core::Scope *scope) {
  node->scope_ = scope;
  node->name_->assign_scope(this, scope);
  node->args_.Apply([this, scope](auto &expr) { expr->assign_scope(this, scope); });
  for (auto &block : node->blocks_) { block.assign_scope(this, scope); }
}

void AssignScope::operator()(ast::Statements *node, core::Scope *scope) {
  node->scope_ = scope;
  for (auto &stmt : node->content_) { stmt->assign_scope(this, scope); }
}

void AssignScope::operator()(ast::StructLiteral *node,
                             core::Scope *scope) {
  node->scope_     = scope;
  node->type_scope = scope->add_child<core::DeclScope>();
  for (auto &a : node->args_) { a.assign_scope(this, node->type_scope.get()); }
  for (auto &f : node->fields_) { f.assign_scope(this, node->type_scope.get()); }
}

void AssignScope::operator()(ast::StructType *node, core::Scope *scope) {
  for (auto &arg : node->args_) { arg->assign_scope(this, scope); }
}

void AssignScope::operator()(ast::Switch *node, core::Scope *scope) {
  node->scope_ = scope;
  if (node->expr_) { node->expr_->assign_scope(this, scope); }
  for (auto &[body, cond] : node->cases_) {
    body->assign_scope(this, scope);
    cond->assign_scope(this, scope);
  }
}

void AssignScope::operator()(ast::SwitchWhen *node, core::Scope *scope) { UNREACHABLE(); }

void AssignScope::operator()(ast::Terminal *node, core::Scope *scope) {
  node->scope_ = scope;
}

void AssignScope::operator()(ast::Unop *node, core::Scope *scope) {
  node->scope_ = scope;
  node->operand->assign_scope(this, scope);
}

}  // namespace visitor
