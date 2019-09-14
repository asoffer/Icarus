#ifndef ICARUS_IR_CMD_REGISTER_H
#define ICARUS_IR_CMD_REGISTER_H

#include <optional>

#include "ir/basic_block.h"
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

template <typename T>
Reg MakeReg(T t) {
  static_assert(!std::is_same_v<T, Reg>);
  if constexpr (ir::IsRegOr<T>::value) {
    auto& blk = GetBuilder().function()->block(GetBuilder().CurrentBlock());
    blk.cmd_buffer_.append_index<RegisterCmd>();
    blk.cmd_buffer_.append(
        RegisterCmd::MakeControlBits<typename T::type>(t.is_reg()));
    t.apply([&](auto v) { blk.cmd_buffer_.append(v); });
    Reg result = MakeResult<typename T::type>();
    blk.cmd_buffer_.append(result);
    return result;

  } else {
    return MakeReg(RegOr<T>{t});
  }
}

}  // namespace ir

#endif  // ICARUS_IR_CMD_REGISTER_H
