#ifndef ICARUS_IR_ARGUMENTS_H
#define ICARUS_IR_ARGUMENTS_H

#include <string>
#include "ir/results.h"
#include "ir/register.h"
#include "ir/addr.h"

namespace type {
struct Callable;
}  // namespace type

namespace ir {
struct Val;

struct Arguments {
  Arguments() = default;
  Arguments(type::Callable const *c, ir::Results results);

  void append(Val const &val);
  void append(RegisterOr<Addr> reg);
  std::string to_string() const;
  base::untyped_buffer PrepareCallBuffer(ir::Func *fn,
                                         base::untyped_buffer const &regs);

  type::Callable const *type_ = nullptr;
  Results results_;
};
}  // namespace ir

#endif  // ICARUS_IR_ARGUMENTS_H
