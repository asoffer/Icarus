#ifndef ICARUS_IR_CMD_LOAD_H
#define ICARUS_IR_CMD_LOAD_H

#include <string_view>

#include "base/untyped_buffer.h"
#include "ir/cmd/util.h"
#include "ir/reg.h"
#include "ir/reg_or.h"
#include "type/util.h"

namespace ir {

struct LoadCmd {
  constexpr static cmd_index_t index = 14 * 256;

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

  static std::string DebugString(base::untyped_buffer::const_iterator* iter) {
    using base::stringify;
    std::string s;
    control_bits ctrl = iter->read<control_bits>();
    if (ctrl.reg) {
      s.append(stringify(iter->read<Reg>()));
    } else {
      // TODO: Add core::LayoutRequirements so you can skip forward by the
      // appropriate amount without instantiating so many templates.
      PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
        s.append(stringify(
            iter->read<typename std::decay_t<decltype(tag)>::type>()));
      });
    }

    s.append(" ");
    s.append(stringify(iter->read<Reg>()));
    return s;
  }
};

}  // namespace ir

#endif  // ICARUS_IR_CMD_LOAD_H
