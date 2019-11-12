#ifndef ICARUS_IR_CMD_CAST_H
#define ICARUS_IR_CMD_CAST_H

#include <string_view>

#include "ir/cmd/util.h"
#include "ir/cmd_buffer.h"
#include "ir/reg.h"
#include "ir/reg_or.h"

namespace ir {

struct CastCmd {
  constexpr static cmd_index_t index = 26;

  static std::string DebugString(base::untyped_buffer::const_iterator* iter) {
    return "NOT_YET";
  }
};

}  // namespace ir

#endif  // ICARUS_IR_CMD_CAST_H
