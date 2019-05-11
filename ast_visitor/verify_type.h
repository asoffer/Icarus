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
#define ICARUS_AST_NODE_X(name)\
  VerifyResult operator()(ast::name const *node, Context *ctx) const;
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X
};

}  // namespace ast_visitor

#endif  // ICARUS_AST_VISITOR_VERIFY_TYPE_H
