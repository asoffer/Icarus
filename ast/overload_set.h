#ifndef ICARUS_AST_OVERLOAD_SET_H
#define ICARUS_AST_OVERLOAD_SET_H

#include <string>

#include <vector>
#include "type/function.h"
#include "type/typed_value.h"

namespace core {
struct Scope;
}  // namespace core

struct Context;

namespace ast {
struct Expression;

struct OverloadSet
    : public std::vector<type::Typed<Expression *, type::Callable>> {
  OverloadSet() = default;
  OverloadSet(core::Scope *scope, std::string const &id, Context *ctx);

  void add_adl(std::string const &id, type::Type const *t);

  void keep_return(type::Type const *t);
};
}  // namespace ast

#endif  // ICARUS_AST_OVERLOAD_SET_H
