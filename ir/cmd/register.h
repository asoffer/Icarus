#ifndef ICARUS_IR_CMD_REGISTER_H
#define ICARUS_IR_CMD_REGISTER_H

#include <optional>

#include "ir/cmd/util.h"
#include "ir/cmd_buffer.h"
#include "ir/reg.h"

namespace ir {
namespace internal {
struct RegMaker {
  template <typename T>
  T operator()(T t) {
    return t;
  }
};
}  // namespace internal

using RegisterCmd =
    internal::UnaryCmd<27, internal::RegMaker,  //
                       int8_t, int16_t, int32_t, int64_t, uint8_t, uint16_t,
                       uint32_t, uint64_t, FlagsVal, EnumVal, Addr>;

}  // namespace ir

#endif  // ICARUS_IR_CMD_REGISTER_H
