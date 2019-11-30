#ifndef ICARUS_IR_CMD_CALL_H
#define ICARUS_IR_CMD_CALL_H

#include "absl/types/span.h"
#include "ir/cmd/util.h"
#include "ir/reg.h"
#include "ir/reg_or.h"

namespace ir {
struct CallCmd {
  constexpr static cmd_index_t index = 41;

  static std::string DebugString(base::untyped_buffer::const_iterator *iter);
};

}  // namespace ir

#endif  // ICARUS_IR_CMD_CALL_H
