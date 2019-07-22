#ifndef ICARUS_IR_CMD_PRINT_H
#define ICARUS_IR_CMD_PRINT_H

#include <string_view>

#include "ir/basic_block.h"
#include "ir/cmd/util.h"
#include "ir/cmd_buffer.h"

namespace ir {

struct PrintCmd {
  constexpr static cmd_index_t index = 0;

  struct control_bits {
    unsigned primitive_type : 6;
    unsigned reg : 1;
  };
  template <typename T>
  static control_bits MakeControlBits(bool reg) {
    control_bits result;
    result.primitive_type = PrimitiveIndex<T>();
    result.reg            = reg;
    return result;
  }

  static std::optional<BlockIndex> Execute(base::untyped_buffer::iterator* iter,
                                           std::vector<Addr> const& ret_slots,
                                           backend::ExecContext* ctx) {
    auto ctrl = iter->read<control_bits>();
    PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
      using type = typename std::decay_t<decltype(tag)>::type;
      type val =
          ctrl.reg ? ctx->resolve<type>(iter->read<Reg>()) : iter->read<type>();
      if constexpr (std::is_same_v<type, bool>) {
        std::cerr << (val ? "true" : "false");
      } else if constexpr (std::is_same_v<type, uint8_t>) {
        std::cerr << static_cast<unsigned int>(val);
      } else if constexpr (std::is_same_v<type, int8_t>) {
        std::cerr << static_cast<int>(val);
      } else {
        std::cerr << val;
      }
    });
    return std::nullopt;
  }
};

template <typename T>
void Print(T r) {
  auto& blk = GetBlock();
  if constexpr (ir::IsRegOr<T>::value) {
    blk.cmd_buffer_.append_index<PrintCmd>();
    blk.cmd_buffer_.append(
        PrintCmd::MakeControlBits<typename T::type>(r.is_reg_));
    if (r.is_reg_) {
      blk.cmd_buffer_.append(r.reg_);
    } else {
      blk.cmd_buffer_.append(r.val_);
    }
  } else {
    blk.cmd_buffer_.append_index<PrintCmd>();
    blk.cmd_buffer_.append(PrintCmd::MakeControlBits<T>(false));
    blk.cmd_buffer_.append(r);
  }
  DEBUG_LOG("print")(blk.cmd_buffer_.to_string());
}

}  // namespace ir

#endif  // ICARUS_IR_CMD_PRINT_H
