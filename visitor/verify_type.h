#ifndef ICARUS_VISITOR_VERIFY_TYPE_H
#define ICARUS_VISITOR_VERIFY_TYPE_H

#include <iostream>

#include "ast/ast_fwd.h"
#include "base/debug.h"
#include "visitor/verify_result.h"

struct Context;

namespace visitor {

std::ostream& operator<<(std::ostream& os, VerifyResult r);

constexpr bool operator==(VerifyResult lhs, VerifyResult rhs) {
  return lhs.type_ == rhs.type_ && lhs.const_ == rhs.const_;
}

constexpr bool operator!=(VerifyResult lhs, VerifyResult rhs) {
  return !(lhs == rhs);
}

struct VerifyType {
  VerifyResult operator()(ast::Node const *node, Context *ctx) const {
    UNREACHABLE();
  }
#define ICARUS_AST_NODE_X(name)                                                \
  VerifyResult operator()(ast::name const *node, Context *ctx);
#include "ast/node.xmacro.h"
#undef ICARUS_AST_NODE_X

  VerifyResult ConcreteFnLit(ast::FunctionLiteral const *node, Context *ctx);
};

VerifyResult VerifyBody(VerifyType *visitor, ast::FunctionLiteral const *node,
                        Context *ctx);
void VerifyBody(VerifyType *visitor, ast::JumpHandler const *node,
                Context *ctx);

}  // namespace visitor

#endif  // ICARUS_VISITOR_VERIFY_TYPE_H
