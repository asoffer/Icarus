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

  static std::optional<BlockIndex> Execute(base::untyped_buffer::iterator* iter,
                                           std::vector<Addr> const& ret_slots,
                                           backend::ExecContext* ctx) {
    auto ctrl = iter->read<control_bits>();
    PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
      using T = typename std::decay_t<decltype(tag)>::type;
      T val   = ctrl.reg ? ctx->resolve<T>(iter->read<Reg>()) : iter->read<T>();
      if constexpr (std::is_same_v<T, bool>) {
        std::cerr << (val ? "true" : "false");
      } else if constexpr (std::is_same_v<T, uint8_t>) {
        std::cerr << static_cast<unsigned int>(val);
      } else if constexpr (std::is_same_v<T, int8_t>) {
        std::cerr << static_cast<int>(val);
      } else if constexpr (std::is_same_v<T, type::Type const*>) {
        std::cerr << val->to_string();
      } else if constexpr (std::is_same_v<T, Addr>) {
        std::cerr << val.to_string();
      } else if constexpr (std::is_same_v<T, EnumVal>) {
        auto numeric_value = val.value;
        auto enum_type     = iter->read<type::Enum const*>();
        if (auto iter = enum_type->members_.find(numeric_value);
            iter == enum_type->members_.end()) {
          std::cerr << numeric_value;
        } else {
          std::cerr << iter->second;
        }
      } else if constexpr (std::is_same_v<T, FlagsVal>) {
        auto numeric_val = val.value;
        std::vector<std::string> vals;
        auto const& members = iter->read<type::Flags const*>()->members_;

        while (numeric_val != 0) {
          size_t mask = (numeric_val & ((~numeric_val) + 1));
          numeric_val -= mask;
          auto iter = members.find(mask);
          if (iter == members.end()) {
            vals.emplace_back(std::to_string(mask));
          } else {
            vals.emplace_back(iter->second);
          }
        }

        if (vals.empty()) {
          std::cerr << "(empty)";
        } else {
          auto iter = vals.begin();
          std::cerr << *iter++;
          while (iter != vals.end()) { std::cerr << " | " << *iter++; }
        }
      } else {
        std::cerr << val;
      }
    });
    return std::nullopt;
  }

  static void UpdateForInlining(base::untyped_buffer::iterator* iter,
                                Inliner const &inliner) {
    auto ctrl = iter->read<control_bits>();
    if (ctrl.reg) {
      inliner.Inline(&iter->read<Reg>());
    } else {
      // TODO: Add core::LayoutRequirements so you can skip forward by the
      // appropriate amount without instantiating so many templates.
      PrimitiveDispatch(ctrl.primitive_type, [&](auto tag) {
        iter->read<typename std::decay_t<decltype(tag)>::type>();
      });
    }
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
    Print(RegOr<T>(r));
  }
  DEBUG_LOG("print")(blk.cmd_buffer_.to_string());
}

template <typename T,
          typename std::enable_if_t<std::is_same_v<T, EnumVal> ||
                                    std::is_same_v<T, FlagsVal>>* = nullptr>
void Print(RegOr<T> r, type::Type const* t) {
  auto& blk = GetBlock();
  blk.cmd_buffer_.append_index<PrintCmd>();
  blk.cmd_buffer_.append(
      PrintCmd::MakeControlBits<EnumVal>(r.is_reg_));
  if (r.is_reg_) {
    blk.cmd_buffer_.append(r.reg_);
  } else {
    blk.cmd_buffer_.append(r.val_);
  }
  blk.cmd_buffer_.append(t);
  DEBUG_LOG("print")(blk.cmd_buffer_.to_string());
}

}  // namespace ir

#endif  // ICARUS_IR_CMD_PRINT_H
