#ifndef ICARUS_IR_CMD_MISC_H
#define ICARUS_IR_CMD_MISC_H

#include <string>
#include <string_view>
#include <vector>

#include "base/debug.h"
#include "ir/cmd/util.h"
#include "ir/reg_or.h"
#include "type/type.h"

namespace ir {
struct SemanticCmd {
  constexpr static cmd_index_t index = 33;

  enum class Kind : uint8_t { Init, Destroy, Move, Copy };

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);
};

struct LoadSymbolCmd {
  constexpr static cmd_index_t index = 34;

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);
};

struct TypeInfoCmd {
  constexpr static cmd_index_t index = 35;

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);
};

struct AccessCmd {
  constexpr static cmd_index_t index = 37;

  struct control_bits {
    uint8_t is_array : 1;
    uint8_t reg_ptr : 1;
    uint8_t reg_index : 1;
  };
  static control_bits MakeControlBits(bool is_array, bool ptr, bool index) {
    control_bits ctrl_bits;
    ctrl_bits.is_array  = is_array;
    ctrl_bits.reg_ptr   = ptr;
    ctrl_bits.reg_index = index;
    return ctrl_bits;
  }

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);
};

struct VariantAccessCmd {
  constexpr static cmd_index_t index = 38;

  // TODO you store a bool for val vs type and a bool for addr.is_reg(). These
  // should be compresseed.

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);
};

#if defined(ICARUS_DEBUG)
struct DebugIrCmd {
  constexpr static cmd_index_t index =
      (std::numeric_limits<cmd_index_t>::max)();

  static std::string DebugString(base::untyped_buffer::const_iterator* iter) {
    return "debug-ir";
  }
};
#endif  // defined(ICARUS_DEBUG)

}  // namespace ir

#endif  // ICARUS_IR_CMD_MISC_H
