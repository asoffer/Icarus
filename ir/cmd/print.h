#ifndef ICARUS_IR_CMD_PRINT_H
#define ICARUS_IR_CMD_PRINT_H

#include <string_view>

#include "ir/basic_block.h"
#include "ir/cmd/util.h"
#include "ir/cmd_buffer.h"
#include "ir/reg.h"

namespace ir {

struct PrintCmd {
  constexpr static cmd_index_t index = 0;

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

  static BasicBlock const* Execute(base::untyped_buffer::const_iterator* iter,
                                   std::vector<Addr> const& ret_slots,
                                   backend::ExecContext* ctx);

  static std::string DebugString(base::untyped_buffer::const_iterator* iter);
};

template <typename T>
void Print(T r) {
  auto& blk = *GetBuilder().CurrentBlock();
  if constexpr (ir::IsRegOr<T>::value) {
    blk.cmd_buffer_.append_index<PrintCmd>();
    blk.cmd_buffer_.append(
        PrintCmd::MakeControlBits<typename T::type>(r.is_reg()));
    r.apply([&](auto v) { blk.cmd_buffer_.append(v); });
  } else {
    Print(RegOr<T>(r));
  }
}

template <typename T,
          typename std::enable_if_t<std::is_same_v<T, EnumVal> or
                                    std::is_same_v<T, FlagsVal>>* = nullptr>
void Print(RegOr<T> r, type::Type const* t) {
  auto& blk = *GetBuilder().CurrentBlock();
  blk.cmd_buffer_.append_index<PrintCmd>();
  blk.cmd_buffer_.append(PrintCmd::MakeControlBits<T>(r.is_reg()));
  r.apply([&](auto v) { blk.cmd_buffer_.append(v); });
  blk.cmd_buffer_.append(t);
}

}  // namespace ir

#endif  // ICARUS_IR_CMD_PRINT_H
