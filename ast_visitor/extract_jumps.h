#ifndef ICARUS_AST_VISITOR_EXTRACT_JUMPS_H
#define ICARUS_AST_VISITOR_EXTRACT_JUMPS_H

#include <array>
#include <vector>

#include "ast/ast_fwd.h"

namespace ast_visitor {

struct ExtractJumps {
  void operator()(ast::Access const *node);
  void operator()(ast::ArrayLiteral const *node);
  void operator()(ast::ArrayType const *node);
  void operator()(ast::Binop const *node);
  void operator()(ast::BlockLiteral const *node);
  void operator()(ast::BlockNode const *node);
  void operator()(ast::BuiltinFn const *node);
  void operator()(ast::Call const *node);
  void operator()(ast::Cast const *node);
  void operator()(ast::ChainOp const *node);
  void operator()(ast::CommaList const *node);
  void operator()(ast::Declaration const *node);
  void operator()(ast::EnumLiteral const *node);
  void operator()(ast::FunctionLiteral const *node);
  void operator()(ast::Identifier const *node);
  void operator()(ast::Import const *node);
  void operator()(ast::Index const *node);
  void operator()(ast::Interface const *node);
  void operator()(ast::RepeatedUnop const *node);
  void operator()(ast::ScopeLiteral const *node);
  void operator()(ast::ScopeNode const *node);
  void operator()(ast::Statements const *node);
  void operator()(ast::StructLiteral const *node);
  void operator()(ast::StructType const *node);
  void operator()(ast::Switch const *node);
  void operator()(ast::SwitchWhen const *node);
  void operator()(ast::Terminal const *node);
  void operator()(ast::Unop const *node);

  enum class Kind { Return, Yield, Jump };
  std::vector<ast::Expression const *> const &exprs(Kind k) const;

 private:
  std::array<std::vector<ast::Expression const *>, 3> data_;
};

}  // namespace ast_visitor

#endif  // ICARUS_AST_VISITOR_EXTRACT_JUMPS_H
