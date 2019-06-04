#ifndef ICARUS_AST_OVERLOAD_SET_H
#define ICARUS_AST_OVERLOAD_SET_H

#include <string_view>

#include "ast/node.h"
#include "ast/node_span.h"
#include "base/bag.h"
#include "type/function.h"
#include "type/typed_value.h"

namespace core {
struct Scope;
}  // namespace core

struct Context;

namespace ast {
struct Expression;

struct Overload {
  Overload(Expression const *expr, visitor::VerifyResult result)
      : expr(expr), result(std::move(result)) {}
  Expression const *expr;
  visitor::VerifyResult result;
};

struct OverloadSet : public base::bag<Overload> {
  OverloadSet() = default;
  OverloadSet(core::Scope *scope, std::string_view id, Context *ctx);
  OverloadSet(NodeSpan<Declaration const> decls, Context *ctx);

  void add_adl(std::string_view id, type::Type const *t);
};
}  // namespace ast

#endif  // ICARUS_AST_OVERLOAD_SET_H
