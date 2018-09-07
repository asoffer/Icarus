#ifndef ICARUS_IR_REGISTER_H
#define ICARUS_IR_REGISTER_H

#include "base/strong_types.h"

DEFINE_STRONG_INT(IR, Register, i32, std::numeric_limits<i32>::lowest());

namespace IR {
template <typename T>
struct RegisterOr {
  static_assert(!std::is_same_v<Register, T>);
  RegisterOr(Register reg) : reg_(reg), is_reg_(true) {}
  RegisterOr(T val) : val_(val), is_reg_(false) {}

  union {
    Register reg_;
    T val_;
  };
  bool is_reg_;
};

}  // namespace IR

#endif  // ICARUS_IR_REGISTER_H
