#ifndef ICARUS_IR_ARGUMENTS_H
#define ICARUS_IR_ARGUMENTS_H

#include <string>
#include <vector>
#include "base/untyped_buffer.h"
#include "ir/register.h"
#include "type/callable.h"

namespace type {
struct Function;
}  // namespace type

namespace ir {
struct Val;

struct Arguments {
  void append(Val const &val);
  void append(RegisterOr<Addr> reg);
  std::string to_string() const;
  base::untyped_buffer PrepareCallBuffer(ir::Func *fn,
                                         base::untyped_buffer const &regs);

  type::Callable const *type_ = nullptr;
  std::vector<bool> is_reg_;
  base::untyped_buffer args_{0};
};
}  // namespace ir

#endif  // ICARUS_IR_ARGUMENTS_H
