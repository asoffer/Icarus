#ifndef ICARUS_AST_VISITOR_ASSIGN_SCOPE_H
#define ICARUS_AST_VISITOR_ASSIGN_SCOPE_H

#include "core/scope.h"
#include "ast/ast_fwd.h"

namespace ast_visitor {

struct AssignScope {
  void operator()(ast::Access *node, core::Scope *scope);
  void operator()(ast::ArrayLiteral *node, core::Scope *scope);
  void operator()(ast::ArrayType *node, core::Scope *scope);
  void operator()(ast::Binop *node, core::Scope *scope);
  void operator()(ast::BlockLiteral *node, core::Scope *scope);
  void operator()(ast::BlockNode *node, core::Scope *scope);
  void operator()(ast::BuiltinFn *node, core::Scope *scope);
  void operator()(ast::Call *node, core::Scope *scope);
  void operator()(ast::Cast *node, core::Scope *scope);
  void operator()(ast::ChainOp *node, core::Scope *scope);
  void operator()(ast::CommaList *node, core::Scope *scope);
  void operator()(ast::Declaration *node, core::Scope *scope);
  void operator()(ast::EnumLiteral *node, core::Scope *scope);
  void operator()(ast::FunctionLiteral *node, core::Scope *scope);
  void operator()(ast::Identifier *node, core::Scope *scope);
  void operator()(ast::Import *node, core::Scope *scope);
  void operator()(ast::Index *node, core::Scope *scope);
  void operator()(ast::Interface *node, core::Scope *scope);
  void operator()(ast::RepeatedUnop *node, core::Scope *scope);
  void operator()(ast::ScopeLiteral *node, core::Scope *scope);
  void operator()(ast::ScopeNode *node, core::Scope *scope);
  void operator()(ast::Statements *node, core::Scope *scope);
  void operator()(ast::StructLiteral *node, core::Scope *scope);
  void operator()(ast::StructType *node, core::Scope *scope);
  void operator()(ast::Switch *node, core::Scope *scope);
  void operator()(ast::SwitchWhen *node, core::Scope *scope);
  void operator()(ast::Terminal *node, core::Scope *scope);
  void operator()(ast::Unop *node, core::Scope *scope);
};

}  // namespace ast_visitor

#endif  // ICARUS_AST_VISITOR_ASSIGN_SCOPE_H
