#ifndef ICARUS_IR_CMD_PHI_H
#define ICARUS_IR_CMD_PHI_H

#include "ir/cmd/util.h"

namespace ir {
struct PhiCmd {
  constexpr static cmd_index_t index = 36;

  static std::string DebugString(base::untyped_buffer::const_iterator *iter) {
    NOT_YET();
  }
};

}  // namespace ir

#endif  // ICARUS_IR_CMD_PHI_H
