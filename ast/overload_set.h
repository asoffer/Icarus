#ifndef ICARUS_AST_OVERLOAD_SET_H
#define ICARUS_AST_OVERLOAD_SET_H

#include <string>

#include "base/container/vector.h"
#include "type/typed_value.h"
#include "type/function.h"

struct Scope;
struct Context;

namespace ast {
struct Expression;

struct OverloadSet
    : public base::vector<type::Typed<Expression *, type::Callable>> {
  OverloadSet() = default;
  OverloadSet(Scope *scope, std::string const &id, Context *ctx);

  void add_adl(std::string const &id, type::Type const *t);

  void keep_return(type::Type const *t);
};
}  // namespace ast

#endif  // ICARUS_AST_OVERLOAD_SET_H
