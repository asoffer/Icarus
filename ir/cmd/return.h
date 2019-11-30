#ifndef ICARUS_IR_CMD_RETURN_H
#define ICARUS_IR_CMD_RETURN_H

#include "absl/strings/str_cat.h"
#include "base/untyped_buffer.h"
#include "ir/cmd/util.h"
#include "type/jump.h"

namespace ir {

struct ReturnCmd {
  constexpr static cmd_index_t index = 28;

  struct control_bits {
    uint8_t primitive_type : 6;
    uint8_t reg : 1;
    uint8_t only_get : 1;
  };

  template <typename T>
  static control_bits MakeControlBits(bool reg, bool only_get) {
    control_bits result;
    if (only_get) {
      result.primitive_type = 0;
      result.reg            = 0;
      result.only_get       = 1;
    } else {
      result.primitive_type = PrimitiveIndex<T>();
      result.reg            = reg;
      result.only_get       = 0;
    }
    return result;
  };

  static std::string DebugString(base::untyped_buffer::const_iterator* iter) {
    using base::stringify;
    auto ctrl  = iter->read<control_bits>();
    uint16_t n = iter->read<uint16_t>();

    if (ctrl.only_get) {
      return absl::StrCat(stringify(iter->read<Reg>()), " = get-ret ", n);
    }

    std::string s;
    if (ctrl.reg) {
      s = absl::StrCat(n, " ", stringify(iter->read<Reg>()));
    } else {
      // TODO: Add core::LayoutRequirements so you can skip forward by the
      // appropriate amount without instantiating so many templates.
      PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
        s = absl::StrCat(
            n, " ",
            stringify(
                iter->read<typename std::decay_t<decltype(tag)>::type>()));
      });
    }
    return s;
  }
};

}  // namespace ir

#endif  // ICARUS_IR_CMD_RETURN_H
