#ifndef ICARUS_AST_VISITOR_VERIFY_TYPE_H
#define ICARUS_AST_VISITOR_VERIFY_TYPE_H

#include <iostream>

#include "base/debug.h"
#include "ast/ast_fwd.h"

struct Context;

namespace type {
struct Type;
}  // namespace type

namespace ast_visitor {
struct VerifyResult {
  type::Type const *type_;
  bool const_;

  constexpr VerifyResult() : type_(nullptr), const_(false) {}
  constexpr VerifyResult(type::Type const *t, bool b) : type_(t), const_(b) {}

  // TODO you could actually pass some information through successfully. Like
  // maybe there's a type error but you do at least know it's a constant.
  static constexpr VerifyResult Error() { return VerifyResult{nullptr, false}; }
  static constexpr VerifyResult Constant(type::Type const *t) {
    return VerifyResult{t, true};
  }
  static constexpr VerifyResult NonConstant(type::Type const *t) {
    return VerifyResult{t, false};
  }

  explicit operator bool() const { return type_ != nullptr; }
  bool ok() const { return type_ != nullptr; }
  VerifyResult operator*() const { return *this; }
};

std::ostream& operator<<(std::ostream& os, VerifyResult r);

constexpr bool operator==(VerifyResult lhs, VerifyResult rhs) {
  return lhs.type_ == rhs.type_ && lhs.const_ == rhs.const_;
}

constexpr bool operator!=(VerifyResult lhs, VerifyResult rhs) {
  return !(lhs == rhs);
}

struct VerifyType {
  VerifyResult operator()(ast::Access const *node, Context *ctx) const;
  VerifyResult operator()(ast::ArrayLiteral const *node, Context *ctx) const;
  VerifyResult operator()(ast::ArrayType const *node, Context *ctx) const;
  VerifyResult operator()(ast::Binop const *node, Context *ctx) const;
  VerifyResult operator()(ast::BlockLiteral const *node, Context *ctx) const;
  VerifyResult operator()(ast::BlockNode const *node, Context *ctx) const;
  VerifyResult operator()(ast::BuiltinFn const *node, Context *ctx) const;
  VerifyResult operator()(ast::Call const *node, Context *ctx) const;
  VerifyResult operator()(ast::Cast const *node, Context *ctx) const;
  VerifyResult operator()(ast::ChainOp const *node, Context *ctx) const;
  VerifyResult operator()(ast::CommaList const *node, Context *ctx) const;
  VerifyResult operator()(ast::Declaration const *node, Context *ctx) const;
  VerifyResult operator()(ast::EnumLiteral const *node, Context *ctx) const;
  VerifyResult operator()(ast::FunctionLiteral const *node, Context *ctx) const;
  VerifyResult operator()(ast::Identifier const *node, Context *ctx) const;
  VerifyResult operator()(ast::Import const *node, Context *ctx) const;
  VerifyResult operator()(ast::MatchDeclaration const *node,
                          Context *ctx) const;
  VerifyResult operator()(ast::Index const *node, Context *ctx) const;
  VerifyResult operator()(ast::Interface const *node, Context *ctx) const;
  VerifyResult operator()(ast::RepeatedUnop const *node, Context *ctx) const;
  VerifyResult operator()(ast::ScopeLiteral const *node, Context *ctx) const;
  VerifyResult operator()(ast::ScopeNode const *node, Context *ctx) const;
  VerifyResult operator()(ast::Statements const *node, Context *ctx) const;
  VerifyResult operator()(ast::StructLiteral const *node, Context *ctx) const;
  VerifyResult operator()(ast::StructType const *node, Context *ctx) const;
  VerifyResult operator()(ast::Switch const *node, Context *ctx) const;
  VerifyResult operator()(ast::SwitchWhen const *node, Context *ctx) const {
    UNREACHABLE();
  }
  VerifyResult operator()(ast::Terminal const *node, Context *ctx) const;
  VerifyResult operator()(ast::Unop const *node, Context *ctx) const;
};

}  // namespace ast_visitor

#endif  // ICARUS_AST_VISITOR_VERIFY_TYPE_H
