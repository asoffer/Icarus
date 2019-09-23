#ifndef ICARUS_AST_OVERLOAD_SET_H
#define ICARUS_AST_OVERLOAD_SET_H

#include <string_view>

#include "base/bag.h"
#include "base/ptr_span.h"
#include "visitor/traditional_compilation.h"
#include "visitor/verify_result.h"

namespace type {
struct Type;
}  // namespace type

namespace core {
struct Scope;
}  // namespace core

namespace visitor {
struct TraditionalCompilation;
}  // namespace visitor

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
  OverloadSet(core::Scope *scope, std::string_view id,
              visitor::TraditionalCompilation *visitor);
  OverloadSet(base::PtrSpan<Declaration const> decls,
              visitor::TraditionalCompilation *visitor);

  void add_adl(std::string_view id, type::Type const *t);
};
}  // namespace ast

#endif  // ICARUS_AST_OVERLOAD_SET_H
