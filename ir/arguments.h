#ifndef ICARUS_IR_ARGUMENTS_H
#define ICARUS_IR_ARGUMENTS_H

#include <string>
#include "ir/addr.h"
#include "ir/register.h"
#include "ir/results.h"
#include "type/callable.h"

namespace type {
struct Callable;
}  // namespace type

namespace ir {
struct Val;

struct Arguments {
  Arguments() = default;
  Arguments(type::Callable const *c, Results results);

  void append(ir::Results val);
  void append(RegisterOr<Addr> reg);
  std::string to_string() const;
  base::untyped_buffer PrepareCallBuffer(ir::Func *fn,
                                         base::untyped_buffer const &regs);

  type::Callable const *type_ = nullptr;
  Results results_;
};
}  // namespace ir

#endif  // ICARUS_IR_ARGUMENTS_H
