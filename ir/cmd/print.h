#ifndef ICARUS_IR_CMD_PRINT_H
#define ICARUS_IR_CMD_PRINT_H

#include <string_view>

#include "ir/cmd/util.h"
#include "ir/reg.h"

namespace ir {

struct PrintCmd {
  constexpr static cmd_index_t index = 12 * 256;

  struct control_bits {
    uint8_t primitive_type : 6;
    uint8_t reg : 1;
  };
  template <typename T>
  static control_bits MakeControlBits(bool reg) {
    control_bits result;
    result.primitive_type = PrimitiveIndex<T>();
    result.reg            = reg;
    return result;
  }

  static std::string DebugString(base::untyped_buffer::const_iterator* iter);
};

}  // namespace ir

#endif  // ICARUS_IR_CMD_PRINT_H
